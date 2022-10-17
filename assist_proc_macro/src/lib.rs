use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use color_eyre::eyre::{eyre, Result};
use convert_case::{Case, Casing};
use indexmap::IndexMap;
use proc_macro::*;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse::{Error, Parse, ParseStream, Result as ParseResult},
    parse_macro_input,
    punctuated::Punctuated,
    Ident, Token, Type,
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
    overrides: HashMap<PathBuf, Vec<OverrideProperty>>,
}

impl Parse for AssistMacro {
    fn parse(input: ParseStream) -> ParseResult<Self> {
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

        let mut overrides = HashMap::new();
        if input.parse::<Token![where]>().is_ok() {
            let overs: Punctuated<Override, Token![,]> = Punctuated::parse_terminated(input)?;
            for o in overs.into_iter() {
                overrides.insert(o.path, o.properties.into_iter().collect());
            }
        }

        Ok(AssistMacro {
            alias,
            path,
            mod_name,
            overrides,
        })
    }
}

struct Override {
    path: PathBuf,
    properties: Punctuated<OverrideProperty, Token![+]>,
}

impl Parse for Override {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let path = input.parse::<syn::LitStr>()?.value().into();
        input.parse::<Token![:]>()?;
        let properties = Punctuated::parse_separated_nonempty(input)?;
        Ok(Override { path, properties })
    }
}

enum OverrideProperty {
    Single,
    Virtual,
    WithCache,
    NoCache,
    WithConst,
    NoConst,
    WithIo,
    NoIo,
    Transpose(TransposeType),
    Serde(Box<Type>),
}

enum TransposeType {
    Required,
    Option,
    Default,
}

impl Parse for OverrideProperty {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let ident = input.parse::<syn::Ident>()?;

        let value = match ident.to_string().as_str() {
            "Single" => OverrideProperty::Single,
            "Virtual" => OverrideProperty::Virtual,
            "WithCache" => OverrideProperty::WithCache,
            "NoCache" => OverrideProperty::NoCache,
            "WithConst" => OverrideProperty::WithConst,
            "NoConst" => OverrideProperty::NoConst,
            "WithIo" => OverrideProperty::WithIo,
            "NoIo" => OverrideProperty::NoIo,
            "Transpose" => {
                let mut kind = TransposeType::Required;
                if input.parse::<Token![<]>().is_ok() {
                    let ident = input.parse::<syn::Ident>()?;
                    kind = match ident.to_string().as_str() {
                        "Required" => TransposeType::Required,
                        "Default" => TransposeType::Default,
                        "Option" => TransposeType::Option,
                        _ => return Err(Error::new(ident.span(), "Unrecognized transpose type")),
                    };
                    input.parse::<Token![>]>()?;
                }
                OverrideProperty::Transpose(kind)
            }
            "Serde" => {
                input.parse::<Token![<]>()?;
                let r#type = input.parse::<Type>()?;
                input.parse::<Token![>]>()?;
                OverrideProperty::Serde(Box::new(r#type))
            }
            _ => return Err(Error::new(ident.span(), "Unrecognized override for path")),
        };

        Ok(value)
    }
}

fn to_ident(x: &str) -> Ident {
    quote::format_ident!("{}", x)
}

fn generate_modules(
    assist: &Ident,
    overrides: &HashMap<PathBuf, Vec<OverrideProperty>>,
    virtuals: HashSet<PathBuf>,
    path: &Path,
    mod_name: &str,
) -> TokenStream2 {
    if virtuals.contains(path) {
        generate_virtual_module(assist, overrides, virtuals, path, mod_name).unwrap()
    } else {
        generate_dir_module(assist, overrides, virtuals, path, mod_name).unwrap()
    }
}

enum MainType {
    None,

    #[cfg(feature = "image")]
    Image,

    #[cfg(feature = "serde_json")]
    Json,

    #[cfg(feature = "serde_toml")]
    Toml,

    #[cfg(feature = "serde_yaml")]
    Yaml,
}

impl MainType {
    fn _match_ext(files: &[impl AsRef<Path>], exts: &[&str]) -> bool {
        !files.is_empty()
            && files.iter().all(|path| {
                exts.iter()
                    .any(|ext| Some(ext.as_ref()) == path.as_ref().extension())
            })
    }

    pub fn of(_files: &[impl AsRef<Path>]) -> Self {
        #[cfg(all(feature = "disable_const_data", feature = "disable_disk_io"))]
        {
            return MainType::None;
        }
        #[cfg(not(all(feature = "disable_const_data", feature = "disable_disk_io")))]
        {
            #[cfg(feature = "image")]
            {
                if !_files.is_empty()
                    && _files
                        .iter()
                        .all(|p| image::ImageFormat::from_path(p).is_ok())
                {
                    return MainType::Image;
                }
            }
            #[cfg(feature = "serde_json")]
            {
                if Self::_match_ext(_files, &["json"]) {
                    return MainType::Json;
                }
            }
            #[cfg(feature = "serde_toml")]
            {
                if Self::_match_ext(_files, &["toml"]) {
                    return MainType::Toml;
                }
            }
            #[cfg(feature = "serde_yaml")]
            {
                if Self::_match_ext(_files, &["yaml", "yml"]) {
                    return MainType::Yaml;
                }
            }
        }
        MainType::None
    }
}

fn generate_dir_module(
    assist: &Ident,
    overrides: &HashMap<PathBuf, Vec<OverrideProperty>>,
    virtuals: HashSet<PathBuf>,
    path: &Path,
    mod_name: &str,
) -> Result<TokenStream2> {
    let enum_name = to_ident(&mod_name.to_case(Case::Pascal));
    let mod_name = to_ident(mod_name);

    let virtuals_clone = virtuals.clone();

    let sub_modules = ignore::WalkBuilder::new(path)
        .max_depth(Some(1))
        .filter_entry(move |entry| {
            entry
                .file_type()
                .map(|f| f.is_dir() || virtuals_clone.contains(entry.path()))
                .unwrap_or(false)
        })
        .build()
        .skip(1)
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.into_path())
        .map(move |path| {
            let mod_name = path
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_case(Case::Snake);
            generate_modules(assist, overrides, virtuals.clone(), &path, &mod_name)
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

    let variant_str_names = files
        .iter()
        .map(|filepath| {
            let filepath = filepath.strip_prefix(path).unwrap().file_stem().unwrap();
            let variant_name = filepath.to_string_lossy().as_ref().to_case(Case::Pascal);
            variant_name
        })
        .collect::<Vec<_>>();
    let variant_names = variant_str_names
        .iter()
        .map(|s| to_ident(s))
        .collect::<Vec<_>>();
    let variant_names = &variant_names;

    let variant_paths = files
        .iter()
        .map(|filepath| filepath.to_string_lossy())
        .collect::<Vec<_>>();
    let variant_paths = &variant_paths;

    let has_disk = cfg!(not(feature = "disable_disk_io"));
    let has_const = cfg!(not(feature = "disable_const_data"));

    let base_asset_impls = if !(has_const || has_disk) {
        None
    } else {
        let [known_try_bytes, known_try_string] = match has_const {
            true => [
                quote!(Cow::Borrowed(c.const_bytes())),
                quote!(Cow::Borrowed(c.try_const_str()?)),
            ],
            false => [
                quote!(Cow::Owned(c.load_bytes())),
                quote!(Cow::Owned(c.try_load_string()?)),
            ],
        };
        let [unknown_try_bytes, try_bytes_modified, try_load_bytes, unknown_try_string] =
            match has_disk {
                true => [
                    quote!(Cow::Owned(#assist::internal::fetch_bytes(_index)?)),
                    quote!(#assist::internal::fetch_bytes_modified(self.try_path()?.as_ref(), previous_modified)),
                    quote!(Ok(std::fs::read(self.try_path()?)?)),
                    quote!(Cow::Owned(#assist::internal::fetch_string(_index)?)),
                ],
                false => [
                    quote!(Err(#assist::AssistError::DiskIoDisabled)?),
                    quote!(Ok(match previous_modified {
                        None => Some((self.try_bytes()?.into_owned(), SystemTime::UNIX_EPOCH)),
                        _ => None,
                    })),
                    quote!(Err(#assist::AssistError::DiskIoDisabled)?),
                    quote!(Err(#assist::AssistError::DiskIoDisabled)?),
                ],
            };

        Some(quote!(
            impl BytesAsset for #enum_name {
                fn try_bytes(&self) -> #assist::Result<Cow<'static, [u8]>> {
                    Ok(match *self {
                        Self::_Path(_index) => #unknown_try_bytes,
                        c => #known_try_bytes,
                    })
                }

                fn try_bytes_modified(&self, previous_modified: Option<SystemTime>) -> #assist::Result<Option<(Vec<u8>, SystemTime)>> {
                    #try_bytes_modified
                }

                fn try_load_bytes(&self) -> #assist::Result<Vec<u8>> {
                    #try_load_bytes
                }
            }

            impl StrAsset for #enum_name {
                fn try_string(&self) -> #assist::Result<Cow<'static, str>> {
                    Ok(match *self {
                        Self::_Path(_index) => #unknown_try_string,
                        c => #known_try_string,
                    })
                }
            }
        ))
    };

    let main_type = MainType::of(&files);
    let serde_asset_impl: Option<TokenStream2> = match main_type {
        #[cfg(feature = "serde_json")]
        MainType::Json => Some(quote!(
            impl #assist::SerdeAsset for #enum_name {
                fn deserialize<'b, T: for<'a> #assist::load::Deserialize<'a>>(bytes: Cow<'b, [u8]>) -> #assist::Result<T> {
                    #assist::load::load_json(bytes)
                }
            }
        )),
        #[cfg(feature = "serde_toml")]
        MainType::Toml => Some(quote!(
            impl #assist::SerdeAsset for #enum_name {
                fn deserialize<'b, T: for<'a> #assist::load::Deserialize<'a>>(bytes: Cow<'b, [u8]>) -> #assist::Result<T> {
                    #assist::load::load_toml(bytes)
                }
            }
        )),
        #[cfg(feature = "serde_yaml")]
        MainType::Yaml => Some(quote!(
            impl #assist::SerdeAsset for #enum_name {
                fn deserialize<'b, T: for<'a> #assist::load::Deserialize<'a>>(bytes: Cow<'b, [u8]>) -> #assist::Result<T> {
                    #assist::load::load_yaml(bytes)
                }
            }
        )),
        _ => None,
    };

    let specific_type: Option<&Type> = {
        overrides.get(path).and_then(|props| {
            props
                .iter()
                .filter_map(|p| match p {
                    OverrideProperty::Serde(ty) => Some(ty.as_ref()),
                    _ => None,
                })
                .next()
        })
    };

    let _serde_impl = specific_type.map(|ty| {
        quote!(
            impl #assist::Asset for #enum_name {
                type Value = #ty;

                fn load(bytes: std::borrow::Cow<[u8]>) -> #assist::Result<Self::Value> {
                    <Self as #assist::SerdeAsset>::deserialize(bytes)
                }
            }
        )
    });
    let asset_impl: Option<TokenStream2> = match main_type {
        #[cfg(feature = "image")]
        MainType::Image => Some(quote!(
            impl #assist::Asset for #enum_name {
                type Value = #assist::load::RgbaImage;

                fn load(bytes: std::borrow::Cow<[u8]>) -> #assist::Result<Self::Value> {
                    #assist::load::load_image(bytes)
                }
            }
        )),

        #[cfg(feature = "serde_json")]
        MainType::Json => _serde_impl,

        #[cfg(feature = "serde_toml")]
        MainType::Toml => _serde_impl,

        #[cfg(feature = "serde_yaml")]
        MainType::Yaml => _serde_impl,

        MainType::None => None,
    };

    let self_cache: Option<TokenStream2> = if cfg!(feature = "self_cached") {
        match main_type {
            #[cfg(feature = "image")]
            MainType::Image => Some(quote!(
                #assist::lazy_static! {
                    pub static ref CACHE: #assist::cache::RwCache<#enum_name, <#enum_name as #assist::Asset>::Value> = #assist::cache::RwCache::new();
                }
            )),
            _ => None,
        }
    } else {
        None
    };

    let self_cache_impl = self_cache.is_some().then(|| {
        quote!(
            impl #assist::CachedAsset for #enum_name {
                fn cache() -> &'static #assist::cache::RwCache<Self, Self::Value> {
                    &CACHE
                }
            }
        )
    });

    let const_data = cfg!(not(feature = "disable_const_data")).then(|| {
        quote!(
            const BYTES: &'static [&'static [u8]] = &[
                #(include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/", #variant_paths)),)*
            ];

            pub const fn const_bytes(self) -> &'static [u8] {
                &Self::BYTES[self.index()]
            }

            pub const fn const_str(self) -> &'static str {
                unsafe { std::str::from_utf8_unchecked(self.const_bytes()) }
            }

            pub const fn try_const_str(self) -> Result<&'static str, std::str::Utf8Error> {
                unsafe { std::str::from_utf8(self.const_bytes()) }
            }
        )
    });

    Ok(quote!(
        pub mod #mod_name {
            use super::*;

            use std::{
                borrow::Cow,
                path::Path,
                time::SystemTime,
            };

            use #assist::{FileAsset, BytesAsset, StrAsset};

            #self_cache

            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[repr(u32)]
            pub enum #enum_name {
                #(#variant_names,)*
                _Path(u32),
            }

            impl #enum_name {
                pub const ALL: &'static [Self] = &[
                    #(Self::#variant_names,)*
                ];

                const PATHS: &'static [&'static str] = &[
                    #(#variant_paths,)*
                ];

                const NAMES: &'static [&'static str] = &[
                    #(#variant_str_names,)*
                ];

                #const_data

                const fn index(self) -> usize {
                    (0x00000000ffffffff & unsafe { std::mem::transmute::<_, u64>(self) }) as usize
                }

                pub const fn const_path(self) -> &'static str {
                    &Self::PATHS[self.index()]
                }

                pub const fn const_name(self) -> &'static str {
                    &Self::NAMES[self.index()]
                }

                // TODO(disk_io)
                pub fn all_variants() -> impl Iterator<Item=Self> {
                    #assist::internal::visit_files(Self::PARENT.as_ref(), Self::from_path)
                }

                fn path_ref(path: &Path) -> Cow<'static, Path> {
                    Self::from_path(path).path()
                }

                // TODO(disk_io)
                pub fn all_paths() -> impl Iterator<Item=Cow<'static, Path>> {
                    #assist::internal::visit_files(Self::PARENT.as_ref(), Self::path_ref)
                }
            }

            impl FileAsset for #enum_name {
                const PARENT: &'static str = #dir_path;

                fn from_const_path(path: &Path) -> Option<Self> {
                    Self::PATHS.iter()
                        .position(|&p| path == <str as AsRef<Path>>::as_ref(p))
                        .map(|index| Self::ALL[index])
                }

                fn from_const_name(name: &str) -> Option<Self> {
                    Self::NAMES.iter()
                        .position(|&n| n == name)
                        .map(|index| Self::ALL[index])
                }

                fn from_path_unchecked(path: &Path) -> Self {
                    Self::from_const_path(path)
                        .unwrap_or_else(|| Self::_Path(#assist::internal::fetch_path_index(path)))
                }

                fn try_path(self) -> #assist::Result<Cow<'static, Path>> {
                    Ok(match self {
                        Self::_Path(index) => Cow::Owned(#assist::internal::fetch_path(index)?),
                        c => Cow::Borrowed(c.const_path().as_ref()),
                    })
                }
            }

            impl #assist::load::Serialize for #enum_name {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: #assist::load::Serializer
                {
                    match *self {
                        Self::_Path(_) => {
                            let path = self.path();
                            let key = #assist::PathedKey::Path { path: path.as_ref() };
                            key.serialize(serializer)
                        },
                        c => {
                            let key = #assist::PathedKey::Known(self.const_name());
                            key.serialize(serializer)
                        }
                    }
                }
            }

            impl<'de> #assist::load::Deserialize<'de> for #enum_name {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: #assist::load::Deserializer<'de>
                {
                    let key: #assist::PathedKey = #assist::PathedKey::deserialize::<D>(deserializer)?;
                    Ok(match key {
                        #assist::PathedKey::Known(name) => Self::from_const_name(name).expect("TODO"),
                        #assist::PathedKey::Path { path } => Self::try_from_path(path).expect("TODO"),
                    })
                }
            }

            #base_asset_impls

            #serde_asset_impl

            #asset_impl

            #self_cache_impl

            #(#sub_modules)*
        }
    ))
}

fn generate_virtual_module(
    assist: &Ident,
    _overrides: &HashMap<PathBuf, Vec<OverrideProperty>>,
    _virtuals: HashSet<PathBuf>,
    path: &Path,
    mod_name: &str,
) -> Result<TokenStream2> {
    let enum_name = to_ident(&mod_name.to_case(Case::Pascal));
    let mod_name = to_ident(mod_name);
    let dir_path = path.to_string_lossy();

    let main_type = MainType::of(&[path]);
    let _source = std::fs::read_to_string(path).unwrap();
    let data: IndexMap<String, Cow<[u8]>> = match main_type {
        #[cfg(feature = "serde_json")]
        MainType::Json => {
            let map: IndexMap<String, &serde_json::value::RawValue> =
                serde_json::from_str(&_source).unwrap();
            map.into_iter()
                .map(|(k, v)| (k, Cow::Borrowed(v.get().as_bytes())))
                .collect()
        }

        #[cfg(feature = "serde_toml")]
        MainType::Toml => {
            let map: IndexMap<String, toml::Value> = toml::from_str(&_source).unwrap();
            map.into_iter()
                .map(|(k, v)| (k, Cow::Owned(toml::to_string(&v).unwrap().into())))
                .collect()
        }

        #[cfg(feature = "serde_yaml")]
        MainType::Yaml => {
            let map: IndexMap<String, serde_yaml::Value> = serde_yaml::from_str(&_source).unwrap();
            map.into_iter()
                .map(|(k, v)| (k, Cow::Owned(serde_yaml::to_string(&v).unwrap().into())))
                .collect()
        }
        _ => Err(eyre!("Need at least one serde feature enabled"))?,
    };

    let variant_names = data.keys().map(|k| to_ident(k)).collect::<Vec<_>>();
    let variant_str_names = data.keys().collect::<Vec<_>>();
    let variant_names = &variant_names;

    let value_bytes = data.values().map(|bs| Literal::byte_string(bs));

    let const_data = cfg!(not(feature = "disable_const_data")).then(|| {
        quote!(
                const BYTES: &'static [&'static [u8]] = &[
                    #(#value_bytes,)*
                ];

                pub const fn const_bytes(self) -> &'static [u8] {
                    &Self::BYTES[self.index()]
                }

                pub const fn const_str(self) -> &'static str {
                    unsafe { std::str::from_utf8_unchecked(self.const_bytes()) }
                }

                pub const fn try_const_str(self) -> Result<&'static str, std::str::Utf8Error> {
                    unsafe { std::str::from_utf8(self.const_bytes()) }
                }
        )
    });

    let try_load_all_fn: TokenStream2 = match main_type {
        #[cfg(feature = "serde_json")]
        MainType::Json => quote!(
            fn try_load_all(bytes: &[u8]) -> #assist::Result<#assist::IndexMap<Self, Vec<u8>>> {
                #assist::load::load_all_json(bytes)
            }
        ),

        #[cfg(feature = "serde_toml")]
        MainType::Toml => quote!(
            fn try_load_all(bytes: &[u8]) -> #assist::Result<#assist::IndexMap<Self, Vec<u8>>> {
                #assist::load::load_all_toml(bytes)
            }
        ),

        #[cfg(feature = "serde_yaml")]
        MainType::Yaml => quote!(
            fn try_load_all(bytes: &[u8]) -> #assist::Result<#assist::IndexMap<Self, Vec<u8>>> {
                #assist::load::load_all_yaml(bytes)
            }
        ),
        _ => Err(eyre!("Need at least one serde feature enabled"))?,
    };

    let has_disk = cfg!(not(feature = "disable_disk_io"));
    let has_const = cfg!(not(feature = "disable_const_data"));

    let base_asset_impls = if !(has_const || has_disk) {
        None
    } else {
        let [known_try_bytes, known_try_string] = match has_const {
            true => [
                quote!(Cow::Borrowed(c.const_bytes())),
                quote!(Cow::Borrowed(c.try_const_str()?)),
            ],
            false => [
                quote!(Cow::Owned(c.load_bytes())),
                quote!(Cow::Owned(c.try_load_string()?)),
            ],
        };
        let [unknown_try_bytes, try_bytes_modified, try_load_bytes, unknown_try_string] =
            match has_disk {
                true => [
                    quote!({
                        let source = std::fs::read(Self::PATH)?;
                        let mut all = Self::try_load_all(&source)?;
                        Cow::Owned(all.remove(self).expect("TODO"))
                    }),
                    quote!(Ok(match #assist::internal::fetch_bytes_modified(Self::PATH.as_ref(), previous_modified)? {
                        Some((bytes, time)) => {
                            let mut all = Self::try_load_all(&bytes)?;
                            Some((all.remove(self).expect("TODO"), time))
                        }
                        None => None,
                    })),
                    quote!(
                        let source = std::fs::read(Self::PATH)?;
                        let mut all = Self::try_load_all(&source)?;
                        Ok(all.remove(self).expect("TODO"))
                    ),
                    quote!({
                        let source = std::fs::read(Self::PATH)?;
                        let mut all = Self::try_load_all(&source)?;
                        Cow::Owned(String::from_utf8(all.remove(self).expect("TODO"))?)
                    }),
                ],
                false => [
                    quote!(Err(#assist::AssistError::DiskIoDisabled)?),
                    quote!(Ok(match previous_modified {
                        None => Some((self.try_bytes()?.into_owned(), SystemTime::UNIX_EPOCH)),
                        _ => None,
                    })),
                    quote!(Err(#assist::AssistError::DiskIoDisabled)?),
                    quote!(Err(#assist::AssistError::DiskIoDisabled)?),
                ],
            };

        Some(quote!(
            impl BytesAsset for #enum_name {
                fn try_bytes(&self) -> #assist::Result<Cow<'static, [u8]>> {
                    Ok(match *self {
                        Self::_Unknown(_index) => #unknown_try_bytes,
                        c => #known_try_bytes,
                    })
                }

                fn try_bytes_modified(&self, previous_modified: Option<SystemTime>) -> #assist::Result<Option<(Vec<u8>, SystemTime)>> {
                    #try_bytes_modified
                }

                fn try_load_bytes(&self) -> #assist::Result<Vec<u8>> {
                    #try_load_bytes
                }
            }

            impl StrAsset for #enum_name {
                fn try_string(&self) -> #assist::Result<Cow<'static, str>> {
                    Ok(match *self {
                        Self::_Unknown(_index) => #unknown_try_string,
                        c => #known_try_string,
                    })
                }
            }
        ))
    };

    Ok(quote!(
        pub mod #mod_name {
            use super::*;

            use std::{
                borrow::Cow,
                path::Path,
                time::SystemTime,
            };

            use #assist::{VirtualAsset, BytesAsset, StrAsset};

            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[repr(u32)]
            pub enum #enum_name {
                #(#variant_names,)*
                _Unknown(u32),
            }

            impl #enum_name {
                pub const ALL: &'static [Self] = &[
                    #(Self::#variant_names,)*
                ];

                const NAMES: &'static [&'static str] = &[
                    #(#variant_str_names,)*
                ];

                #const_data

                const fn index(self) -> usize {
                    (0x00000000ffffffff & unsafe { std::mem::transmute::<_, u64>(self) }) as usize
                }

                pub const fn const_name(self) -> &'static str {
                    &Self::NAMES[self.index()]
                }

                // TODO(disk_io)
                // pub fn all_variants() -> impl Iterator<Item=Self> {
                //     todo!("read keys from actual file")
                // }
            }

            impl VirtualAsset for #enum_name {
                const PATH: &'static str = #dir_path;

                #try_load_all_fn

                fn from_const_name(name: &str) -> Option<Self> {
                    Self::NAMES.iter()
                        .position(|&n| n == name)
                        .map(|index| Self::ALL[index])
                }

                fn from_name(name: &str) -> Self {
                    Self::from_const_name(name)
                        .unwrap_or_else(|| Self::_Unknown(#assist::internal::fetch_name_index(name)))
                }

                fn try_name(self) -> #assist::Result<Cow<'static, str>> {
                    Ok(match self {
                        Self::_Unknown(index) => Cow::Owned(#assist::internal::fetch_name(index)?),
                        c => Cow::Borrowed(c.const_name()),
                    })
                }
            }

            impl #assist::load::Serialize for #enum_name {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: #assist::load::Serializer
                {
                    self.name().serialize(serializer)
                }
            }

            impl<'de> #assist::load::Deserialize<'de> for #enum_name {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: #assist::load::Deserializer<'de>
                {
                    let name: &'de str = <&'de str as #assist::load::Deserialize<'de>>::deserialize::<D>(deserializer)?;
                    Ok(Self::from_name(name))
                }
            }

            #base_asset_impls

            // TODO: SerdeAsset impl
            // TODO: Asset impl if specific type given
        }
    ))
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
    let virtuals = call
        .overrides
        .iter()
        .filter(|entry| {
            entry
                .1
                .iter()
                .any(|prop| matches!(prop, OverrideProperty::Virtual))
        })
        .map(|entry| entry.0.clone())
        .collect::<HashSet<_>>();
    generate_modules(
        &call.alias,
        &call.overrides,
        virtuals,
        &call.path,
        &call.mod_name,
    )
    .into()
}
