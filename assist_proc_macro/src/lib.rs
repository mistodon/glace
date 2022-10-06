use std::path::{Path, PathBuf};

use convert_case::{Case, Casing};
use proc_macro::*;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream, Result},
    parse_macro_input, Ident, Token,
};

#[repr(u32)]
enum SizeTest {
    _Just(u32),
}

#[repr(u32)]
enum LayoutTest {
    Zero,
    One,
    _Path(u32),
}

const SIZE_TEST_ERROR: &str = "Your target's `enum` layout is not compatible with this crate. For this proc macro to work correctly, an enum with a u32 discriminant and a u32 field must be the same size as a u64. Your target may be adding padding, or not respecting the #[repr(u32)] attribute.";

const LAYOUT_TEST_ERROR: &str = "Your target's `enum` layout is not compatible with this crate. For this proc macro to work correctly, an enum with a u32 discriminant and a u32 field must be represented with the discriminant filling the lower 32 bits. Additionally, the discriminant must monotonically increase from 0.";

struct AssistMacro {
    alias: Ident,
    path: PathBuf,
    mod_name: String,
}

impl Parse for AssistMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut alias = to_ident("assist");
        if input.parse::<Token![crate]>().is_ok() {
            alias = to_ident("crate");
            input.parse::<Token![,]>()?;
        } else if let Ok(token) = input.parse::<syn::Ident>() {
            alias = token;
            input.parse::<Token![,]>()?;
        }
        let path = input.parse::<syn::LitStr>()?.value().into();
        input.parse::<Token![,]>()?;
        input.parse::<Token![mod]>()?;
        let mod_name: Ident = input.parse()?;
        let mod_name = mod_name.to_string();

        Ok(AssistMacro {
            alias,
            path,
            mod_name,
        })
    }
}

fn to_ident(x: &str) -> Ident {
    quote::format_ident!("{}", x)
}

fn generate_modules(alias: &Ident, path: &Path, mod_name: &str) -> TokenStream2 {
    let root_enum_name = to_ident(&mod_name.to_case(Case::Pascal));
    let mod_name = to_ident(mod_name);

    let sub_modules = ignore::WalkBuilder::new(path)
        .max_depth(Some(1))
        .filter_entry(|entry| entry.file_type().map(|f| f.is_dir()).unwrap_or(false))
        .build()
        .skip(1)
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.into_path())
        .map(|path| {
            let mod_name = path.file_name().unwrap().to_str().unwrap();
            generate_modules(alias, &path, mod_name)
        })
        .collect::<Vec<_>>();

    let files = ignore::WalkBuilder::new(path)
        .max_depth(Some(1))
        .filter_entry(|entry| {
            let is_file = entry.file_type().map(|f| f.is_file()).unwrap_or(false);
            let skipped = entry
                .path()
                .file_stem()
                .and_then(|s| s.to_str())
                .map(|s| s.starts_with('_'))
                .unwrap_or(false);
            is_file && !skipped
        })
        .build()
        .skip(1)
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.into_path())
        .collect::<Vec<_>>();

    let dir_path = path.to_string_lossy();

    let variant_names = files
        .iter()
        .map(|filepath| {
            let filepath = filepath.strip_prefix(path).unwrap().file_stem().unwrap();
            let variant_name = filepath.to_string_lossy().as_ref().to_case(Case::Pascal);
            to_ident(&variant_name)
        })
        .collect::<Vec<_>>();
    let variant_names = &variant_names;

    let variant_paths = files
        .iter()
        .map(|filepath| filepath.to_string_lossy())
        .collect::<Vec<_>>();
    let variant_paths = &variant_paths;

    let image_asset_impl = cfg!(feature = "image").then(|| {
        quote!(
            impl #alias::ImageAsset for #root_enum_name {
            }
        )
    });

    let serde_json_impl = cfg!(feature = "image").then(|| {
        quote!(
            impl #alias::JsonAsset for #root_enum_name {
            }
        )
    });

    quote!(
        pub mod #mod_name {
            use std::{
                borrow::Cow,
                path::Path,
                time::SystemTime,
            };

            use #alias::{BytesAsset, StrAsset};

            pub const PATH: &'static str = #dir_path;

            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[repr(u32)]
            pub enum #root_enum_name {
                #(#variant_names,)*
                _Path(u32),
            }

            impl #root_enum_name {
                pub const ALL: &'static [Self] = &[
                    #(Self::#variant_names,)*
                ];

                const PATHS: &'static [&'static str] = &[
                    #(#variant_paths,)*
                ];

                const BYTES: &'static [&'static [u8]] = &[
                    #(include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/", #variant_paths)),)*
                ];

                const fn index(self) -> usize {
                    (0x00000000ffffffff & unsafe { std::mem::transmute::<_, u64>(self) }) as usize
                }

                fn path_ref(path: &Path) -> Cow<'static, Path> {
                    Self::from_path(path).path()
                }

                pub const fn const_path(self) -> &'static str {
                    &Self::PATHS[self.index()]
                }

                pub const fn const_bytes(self) -> &'static [u8] {
                    &Self::BYTES[self.index()]
                }

                pub const fn const_str(self) -> &'static str {
                    unsafe { std::str::from_utf8_unchecked(self.const_bytes()) }
                }

                pub const fn try_const_str(self) -> Result<&'static str, std::str::Utf8Error> {
                    unsafe { std::str::from_utf8(self.const_bytes()) }
                }

                pub fn from_const_path(path: &Path) -> Option<Self> {
                    Self::PATHS.iter()
                        .position(|&p| path == <str as AsRef<Path>>::as_ref(p))
                        .map(|index| Self::ALL[index])
                }

                pub fn from_path_unchecked(path: &Path) -> Self {
                    Self::from_const_path(path)
                        .unwrap_or_else(|| Self::_Path(#alias::internal::fetch_path_index(path)))
                }

                pub fn from_path(path: &Path) -> Self {
                    Self::try_from_path(path).unwrap()
                }

                pub fn try_from_path(path: &Path) -> #alias::Result<Self> {
                    #alias::internal::verify_parent(path, PATH.as_ref())?;
                    Ok(Self::from_path_unchecked(path))
                }

                pub fn path(self) -> Cow<'static, Path> {
                    self.try_path().unwrap()
                }

                pub fn try_path(self) -> #alias::Result<Cow<'static, Path>> {
                    Ok(match self {
                        Self::_Path(index) => Cow::Owned(#alias::internal::fetch_path(index)?),
                        c => Cow::Borrowed(c.const_path().as_ref()),
                    })
                }

                pub fn all_variants() -> impl Iterator<Item=Self> {
                    #alias::internal::visit_files(PATH.as_ref(), Self::from_path)
                }

                pub fn all_paths() -> impl Iterator<Item=Cow<'static, Path>> {
                    #alias::internal::visit_files(PATH.as_ref(), Self::path_ref)
                }
            }

            impl BytesAsset for #root_enum_name {
                fn try_bytes(&self) -> #alias::Result<Cow<'static, [u8]>> {
                    Ok(match *self {
                        Self::_Path(index) => Cow::Owned(#alias::internal::fetch_bytes(index)?),
                        c => Cow::Borrowed(c.const_bytes()),
                    })
                }

                fn try_bytes_modified(&self, previous_modified: Option<SystemTime>) -> #alias::Result<Option<(Vec<u8>, SystemTime)>> {
                    #alias::internal::fetch_bytes_modified(self.try_path()?.as_ref(), previous_modified)
                }

                fn try_load_bytes(&self) -> #alias::Result<Vec<u8>> {
                    Ok(std::fs::read(self.try_path()?)?)
                }
            }

            impl StrAsset for #root_enum_name {
                fn try_string(&self) -> #alias::Result<Cow<'static, str>> {
                    Ok(match *self {
                        Self::_Path(index) => Cow::Owned(#alias::internal::fetch_string(index)?),
                        c => Cow::Borrowed(c.try_const_str()?),
                    })
                }
            }

            #image_asset_impl
            #serde_json_impl

            #(#sub_modules)*
        }
    )
}

#[proc_macro]
pub fn assist(stream: TokenStream) -> TokenStream {
    use std::mem::{size_of, transmute};

    {
        assert_eq!(size_of::<SizeTest>(), 8, "{}", SIZE_TEST_ERROR);

        assert_eq!(
            0x00000000ffffffff & unsafe { transmute::<_, u64>(LayoutTest::Zero) },
            0,
            "incompatible discriminant representation: {}",
            LAYOUT_TEST_ERROR
        );
        assert_eq!(
            0x00000000ffffffff & unsafe { transmute::<_, u64>(LayoutTest::One) },
            1,
            "incompatible discriminant representation: {}",
            LAYOUT_TEST_ERROR
        );
        assert_eq!(
            0x00000000ffffffff & unsafe { transmute::<_, u64>(LayoutTest::_Path(123)) },
            2,
            "incompatible discriminant representation: {}",
            LAYOUT_TEST_ERROR
        );
        assert_eq!(
            unsafe { transmute::<_, u64>(LayoutTest::_Path(123)) } >> 32,
            123,
            "incompatible field representation: {}",
            LAYOUT_TEST_ERROR
        );
    }

    let call: AssistMacro = parse_macro_input!(stream as AssistMacro);
    generate_modules(&call.alias, &call.path, &call.mod_name).into()
}
