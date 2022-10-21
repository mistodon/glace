mod codegen;
mod compat_tests;
mod parsing;

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

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

use crate::{
    codegen::{ident, Context, ModType, StringyTokens},
    parsing::{GlaceMacro, Property},
};

fn generate_toplevel_module(context: Arc<Context>, path: &Path, mod_name: &str) -> TokenStream2 {
    let (tokens, _) = generate_modules(context, path, mod_name, None);
    tokens
}

fn generate_modules(
    context: Arc<Context>,
    path: &Path,
    mod_name: &str,
    parents: Option<TokenStream2>,
) -> (TokenStream2, TokenStream2) {
    match context
        .overrides
        .get(path)
        .map(|config| config.mod_type)
        .unwrap_or(ModType::Dir)
    {
        ModType::Dir => generate_dir_module(context, path, mod_name, parents).unwrap(),
        ModType::Single => {
            generate_single_module(context, path, mod_name, parents.unwrap()).unwrap()
        }
        ModType::Virtual => {
            generate_virtual_module(context, path, mod_name, parents.unwrap()).unwrap()
        }
    }
}

#[derive(Debug, Clone, Copy)]
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

    pub fn asset_type(self) -> MainType {
        #[cfg(all(feature = "disable_const_data", feature = "disable_disk_io"))]
        {
            return MainType::None;
        }
        self
    }
}

#[allow(unused_variables)]
fn serde_asset_impl(
    glace: &StringyTokens,
    item_name: &Ident,
    main_type: MainType,
) -> Option<TokenStream2> {
    match main_type {
        #[cfg(feature = "serde_json")]
        MainType::Json => Some(quote!(
            impl #glace::SerdeAsset for #item_name {
                fn deserialize<'b, T: for<'a> #glace::serde::Deserialize<'a>>(bytes: Cow<'b, [u8]>) -> #glace::Result<T> {
                    #glace::_internal::load::load_json(bytes)
                }
            }
        )),

        #[cfg(feature = "serde_toml")]
        MainType::Toml => Some(quote!(
            impl #glace::SerdeAsset for #item_name {
                fn deserialize<'b, T: for<'a> #glace::serde::Deserialize<'a>>(bytes: Cow<'b, [u8]>) -> #glace::Result<T> {
                    #glace::_internal::load::load_toml(bytes)
                }
            }
        )),

        #[cfg(feature = "serde_yaml")]
        MainType::Yaml => Some(quote!(
            impl #glace::SerdeAsset for #item_name {
                fn deserialize<'b, T: for<'a> #glace::serde::Deserialize<'a>>(bytes: Cow<'b, [u8]>) -> #glace::Result<T> {
                    #glace::_internal::load::load_yaml(bytes)
                }
            }
        )),

        _ => None,
    }
}

fn asset_impl(
    glace: &StringyTokens,
    item_name: &Ident,
    main_type: MainType,
    value_type: Option<&StringyTokens>,
) -> Option<TokenStream2> {
    let _serde_impl = value_type.map(|ty| {
        quote!(
            impl #glace::Asset for #item_name {
                type Value = #ty;

                fn load(bytes: Cow<[u8]>) -> #glace::Result<Self::Value> {
                    <Self as #glace::SerdeAsset>::deserialize(bytes)
                }
            }
        )
    });

    match main_type {
        #[cfg(feature = "image")]
        MainType::Image => Some(quote!(
            impl #glace::Asset for #item_name {
                type Value = #glace::image::RgbaImage;

                fn load(bytes: Cow<[u8]>) -> #glace::Result<Self::Value> {
                    #glace::_internal::load::load_image(bytes)
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
    }
}

fn cache_and_impl(
    glace: &StringyTokens,
    item_name: &Ident,
    is_asset: bool,
) -> (Option<TokenStream2>, Option<TokenStream2>) {
    if cfg!(feature = "self_cached") && is_asset {
        let self_cache = quote!(
            #glace::lazy_static::lazy_static! {
                pub static ref CACHE: #glace::cache::RwCache<#item_name, <#item_name as #glace::Asset>::Value> = #glace::cache::RwCache::new();
            }
        );
        let cached_impl = quote!(
            impl #glace::CachedAsset for #item_name {
                fn cache() -> &'static #glace::cache::RwCache<Self, Self::Value> {
                    &CACHE
                }
            }
        );
        (Some(self_cache), Some(cached_impl))
    } else {
        (None, None)
    }
}

fn generate_dir_module(
    context: Arc<Context>,
    path: &Path,
    mod_name: &str,
    parents: Option<TokenStream2>,
) -> Result<(TokenStream2, TokenStream2)> {
    let glace = &context.alias;
    let enum_str_name = mod_name.to_case(Case::Pascal);
    let enum_name = ident(&enum_str_name);
    let mod_name = ident(mod_name);

    let next_parents = match &parents {
        Some(parents) => quote!(#parents::#mod_name),
        None => quote!(super),
    };
    let return_parents = next_parents.clone();

    let dir_context = Arc::clone(&context);
    let gen_context = Arc::clone(&context);
    let file_context = Arc::clone(&context);

    let (sub_modules, prelude_contents): (Vec<_>, Vec<_>) = ignore::WalkBuilder::new(path)
        .max_depth(Some(1))
        .sort_by_file_name(std::ffi::OsStr::cmp)
        .filter_entry(move |entry| {
            entry
                .file_type()
                .map(|f| {
                    f.is_dir()
                        || dir_context.is_single(entry.path())
                        || dir_context.is_virtual(entry.path())
                })
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
            generate_modules(
                Arc::clone(&gen_context),
                &path,
                &mod_name,
                Some(next_parents.clone()),
            )
        })
        .unzip();

    let files = ignore::WalkBuilder::new(path)
        .max_depth(Some(1))
        .sort_by_file_name(std::ffi::OsStr::cmp)
        .filter_entry(move |entry| {
            let is_file = entry.file_type().map(|f| f.is_file()).unwrap_or(false);
            let skipped = entry
                .path()
                .file_stem()
                .and_then(|s| s.to_str())
                .map(|s| s.starts_with('_'))
                .unwrap_or(false);
            is_file
                && !skipped
                && !file_context.is_single(entry.path())
                && !file_context.is_virtual(entry.path())
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
        .map(|s| ident(s))
        .collect::<Vec<_>>();
    let variant_names = &variant_names;

    let variant_paths = files
        .iter()
        .map(|filepath| filepath.to_string_lossy())
        .collect::<Vec<_>>();
    let variant_paths = &variant_paths;

    let has_disk = cfg!(not(feature = "disable_disk_io"));
    let has_const = cfg!(not(feature = "disable_const_data"));

    let base_asset_impls = (has_const || has_disk).then(|| {
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
                    quote!(Cow::Owned(#glace::_internal::fetch_bytes(_index)?)),
                    quote!(#glace::_internal::fetch_bytes_modified(self.try_path()?.as_ref(), previous_modified)),
                    quote!(Ok(std::fs::read(self.try_path()?)?)),
                    quote!(Cow::Owned(#glace::_internal::fetch_string(_index)?)),
                ],
                false => [
                    quote!(Err(#glace::GlaceError::DiskIoDisabled)?),
                    quote!(Ok(match previous_modified {
                        None => Some((self.try_bytes()?.into_owned(), SystemTime::UNIX_EPOCH)),
                        _ => None,
                    })),
                    quote!(Err(#glace::GlaceError::DiskIoDisabled)?),
                    quote!(Err(#glace::GlaceError::DiskIoDisabled)?),
                ],
            };

        quote!(
            impl BytesAsset for #enum_name {
                fn try_bytes(&self) -> #glace::Result<Cow<'static, [u8]>> {
                    Ok(match *self {
                        Self::_Path(_index) => #unknown_try_bytes,
                        c => #known_try_bytes,
                    })
                }

                fn try_bytes_modified(&self, previous_modified: Option<SystemTime>) -> #glace::Result<Option<(Vec<u8>, SystemTime)>> {
                    #try_bytes_modified
                }

                fn try_load_bytes(&self) -> #glace::Result<Vec<u8>> {
                    #try_load_bytes
                }
            }

            impl StrAsset for #enum_name {
                fn try_string(&self) -> #glace::Result<Cow<'static, str>> {
                    Ok(match *self {
                        Self::_Path(_index) => #unknown_try_string,
                        c => #known_try_string,
                    })
                }
            }
        )
    });

    let main_type = MainType::of(&files).asset_type();
    let serde_asset_impl = serde_asset_impl(glace, &enum_name, main_type);

    let (specific_type, edres_tokens): (Option<StringyTokens>, Option<TokenStream2>) = {
        let overridden = context.explicit_type(path);
        if overridden.is_some() {
            (overridden, None)
        } else {
            #[cfg(any(feature = "edres_json", feature = "edres_toml", feature = "edres_yaml"))]
            {
                let value_str_name = format!("{}Value", enum_str_name);
                let value_name = ident(&value_str_name);
                let format: Option<edres::Format> = match main_type {
                    #[cfg(feature = "edres_json")]
                    MainType::Json => Some(edres::Format::Json),

                    #[cfg(feature = "edres_toml")]
                    MainType::Toml => Some(edres::Format::Toml),

                    #[cfg(feature = "edres_yaml")]
                    MainType::Yaml => Some(edres::Format::Yaml),

                    _ => None,
                };

                if let Some(format) = format {
                    let tokens = Some(edres::codegen::define_structs_from_file_contents(
                        path,
                        &value_str_name,
                        Some(format),
                        &edres::Options {
                            serde_support: edres::SerdeSupport::Yes,
                            structs: edres::StructOptions {
                                derived_traits: vec![
                                    "Debug".into(),
                                    "Clone".into(),
                                    "PartialEq".into(),
                                ]
                                .into(),
                                ..edres::StructOptions::minimal()
                            },
                            ..edres::Options::minimal()
                        },
                    )?);

                    #[cfg(not(any(
                        feature = "edres_json",
                        feature = "edres_toml",
                        feature = "edres_yaml"
                    )))]
                    let tokens = None;

                    let specific_type = StringyTokens::from(Type::Verbatim(quote!(#value_name)));

                    (Some(specific_type), tokens)
                } else {
                    (None, None)
                }
            }
            #[cfg(not(any(
                feature = "edres_json",
                feature = "edres_toml",
                feature = "edres_yaml"
            )))]
            {
                (None, None)
            }
        }
    };
    let specific_type: Option<&StringyTokens> = specific_type.as_ref();
    let edres_attrs = edres_tokens
        .is_some()
        .then(|| quote!(#![allow(clippy::derive_partial_eq_without_eq)]));

    let asset_impl = asset_impl(glace, &enum_name, main_type, specific_type);

    let (self_cache, self_cache_impl) = cache_and_impl(glace, &enum_name, asset_impl.is_some());

    let const_data: Option<TokenStream2> = cfg!(not(feature = "disable_const_data")).then(|| {
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

    let include_prelude = parents.is_none();
    let prelude_module = include_prelude.then(|| {
        quote!(pub mod prelude {
            #(pub use #prelude_contents;)*
        })
    });

    let tokens = quote!(
        pub mod #mod_name {
            #edres_attrs

            #prelude_module

            use super::*;

            use std::{
                borrow::Cow,
                path::Path,
                time::SystemTime,
            };

            use #glace::{FileAsset, BytesAsset, StrAsset};

            #edres_tokens

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
                    #glace::_internal::visit_files(Self::PARENT.as_ref(), Self::from_path)
                }

                fn path_ref(path: &Path) -> Cow<'static, Path> {
                    Self::from_path(path).path()
                }

                // TODO(disk_io)
                pub fn all_paths() -> impl Iterator<Item=Cow<'static, Path>> {
                    #glace::_internal::visit_files(Self::PARENT.as_ref(), Self::path_ref)
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
                        .unwrap_or_else(|| Self::_Path(#glace::_internal::fetch_path_index(path)))
                }

                fn try_path(self) -> #glace::Result<Cow<'static, Path>> {
                    Ok(match self {
                        Self::_Path(index) => Cow::Owned(#glace::_internal::fetch_path(index)?),
                        c => Cow::Borrowed(c.const_path().as_ref()),
                    })
                }
            }

            impl #glace::serde::Serialize for #enum_name {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: #glace::serde::Serializer
                {
                    match *self {
                        Self::_Path(_) => {
                            let path = self.path();
                            let key = #glace::PathedKey::Path { path: path.as_ref() };
                            key.serialize(serializer)
                        },
                        c => {
                            let key = #glace::PathedKey::Known(self.const_name());
                            key.serialize(serializer)
                        }
                    }
                }
            }

            impl<'de> #glace::serde::Deserialize<'de> for #enum_name {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: #glace::serde::Deserializer<'de>
                {
                    let key: #glace::PathedKey = #glace::PathedKey::deserialize::<D>(deserializer)?;
                    Ok(match key {
                        #glace::PathedKey::Known(name) => Self::from_const_name(name).expect("TODO"),
                        #glace::PathedKey::Path { path } => Self::try_from_path(path).expect("TODO"),
                    })
                }
            }

            #base_asset_impls

            #serde_asset_impl

            #asset_impl

            #self_cache_impl

            #(#sub_modules)*
        }
    );

    Ok((tokens, quote!(#return_parents::#enum_name)))
}

fn generate_virtual_module(
    context: Arc<Context>,
    path: &Path,
    mod_name: &str,
    parents: TokenStream2,
) -> Result<(TokenStream2, TokenStream2)> {
    let glace = &context.alias;
    let enum_str_name = mod_name.to_case(Case::Pascal);
    let enum_name = ident(&enum_str_name);
    let mod_name = ident(mod_name);
    let dir_path = path.to_string_lossy();

    let main_type = MainType::of(&[path]);
    let asset_type = main_type.asset_type();
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

    let variant_names = data.keys().map(|k| ident(k)).collect::<Vec<_>>();
    let variant_str_names = data.keys().collect::<Vec<_>>();
    let variant_names = &variant_names;

    let value_bytes = data.values().map(|bs| Literal::byte_string(bs));

    let const_data: Option<TokenStream2> = cfg!(not(feature = "disable_const_data")).then(|| {
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
            fn try_load_all(bytes: &[u8]) -> #glace::Result<#glace::indexmap::IndexMap<Self, Vec<u8>>> {
                #glace::_internal::load::load_all_json(bytes)
            }
        ),

        #[cfg(feature = "serde_toml")]
        MainType::Toml => quote!(
            fn try_load_all(bytes: &[u8]) -> #glace::Result<#glace::indexmap::IndexMap<Self, Vec<u8>>> {
                #glace::_internal::load::load_all_toml(bytes)
            }
        ),

        #[cfg(feature = "serde_yaml")]
        MainType::Yaml => quote!(
            fn try_load_all(bytes: &[u8]) -> #glace::Result<#glace::indexmap::IndexMap<Self, Vec<u8>>> {
                #glace::_internal::load::load_all_yaml(bytes)
            }
        ),
        _ => Err(eyre!("Need at least one serde feature enabled"))?,
    };

    let has_disk = cfg!(not(feature = "disable_disk_io"));
    let has_const = cfg!(not(feature = "disable_const_data"));

    let base_asset_impls = (has_const || has_disk).then(|| {
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
                    quote!(Ok(match #glace::_internal::fetch_bytes_modified(Self::PATH.as_ref(), previous_modified)? {
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
                    quote!(Err(#glace::GlaceError::DiskIoDisabled)?),
                    quote!(Ok(match previous_modified {
                        None => Some((self.try_bytes()?.into_owned(), SystemTime::UNIX_EPOCH)),
                        _ => None,
                    })),
                    quote!(Err(#glace::GlaceError::DiskIoDisabled)?),
                    quote!(Err(#glace::GlaceError::DiskIoDisabled)?),
                ],
            };

        quote!(
            impl BytesAsset for #enum_name {
                fn try_bytes(&self) -> #glace::Result<Cow<'static, [u8]>> {
                    Ok(match *self {
                        Self::_Unknown(_index) => #unknown_try_bytes,
                        c => #known_try_bytes,
                    })
                }

                fn try_bytes_modified(&self, previous_modified: Option<SystemTime>) -> #glace::Result<Option<(Vec<u8>, SystemTime)>> {
                    #try_bytes_modified
                }

                fn try_load_bytes(&self) -> #glace::Result<Vec<u8>> {
                    #try_load_bytes
                }
            }

            impl StrAsset for #enum_name {
                fn try_string(&self) -> #glace::Result<Cow<'static, str>> {
                    Ok(match *self {
                        Self::_Unknown(_index) => #unknown_try_string,
                        c => #known_try_string,
                    })
                }
            }
        )
    });

    let (specific_type, edres_tokens): (Option<StringyTokens>, Option<TokenStream2>) = {
        let overridden = context.explicit_type(path);
        if overridden.is_some() {
            (overridden, None)
        } else {
            #[cfg(any(feature = "edres_json", feature = "edres_toml", feature = "edres_yaml"))]
            {
                let source = std::fs::read_to_string(path)?;
                let value_str_name = format!("{}Value", enum_str_name);
                let value_name = ident(&value_str_name);
                let value = match main_type {
                    #[cfg(feature = "edres_json")]
                    MainType::Json => {
                        edres::parsing::json::parse_source(&source, &edres::ParseOptions::new())?
                    }

                    #[cfg(feature = "edres_toml")]
                    MainType::Toml => {
                        edres::parsing::toml::parse_source(&source, &edres::ParseOptions::new())?
                    }

                    #[cfg(feature = "edres_yaml")]
                    MainType::Yaml => {
                        edres::parsing::yaml::parse_source(&source, &edres::ParseOptions::new())?
                    }

                    _ => Err(eyre!("Need at least one serde feature enabled"))?,
                };

                let tokens = Some(edres::codegen::define_structs_from_values(
                    &value.assume_struct()?,
                    &value_str_name,
                    &edres::Options {
                        serde_support: edres::SerdeSupport::Yes,
                        structs: edres::StructOptions {
                            derived_traits: vec![
                                "Debug".into(),
                                "Clone".into(),
                                "PartialEq".into(),
                            ]
                            .into(),
                            ..edres::StructOptions::minimal()
                        },
                        ..edres::Options::minimal()
                    },
                )?);

                #[cfg(not(any(
                    feature = "edres_json",
                    feature = "edres_toml",
                    feature = "edres_yaml"
                )))]
                let tokens = None;

                let specific_type = StringyTokens::from(Type::Verbatim(quote!(#value_name)));

                (Some(specific_type), tokens)
            }

            #[cfg(not(any(
                feature = "edres_json",
                feature = "edres_toml",
                feature = "edres_yaml"
            )))]
            {
                (None, None)
            }
        }
    };
    let specific_type: Option<&StringyTokens> = specific_type.as_ref();
    let edres_attrs = edres_tokens
        .is_some()
        .then(|| quote!(#![allow(clippy::derive_partial_eq_without_eq)]));

    let asset_impl = asset_impl(glace, &enum_name, asset_type, specific_type);
    let serde_asset_impl = serde_asset_impl(glace, &enum_name, asset_type);

    let (self_cache, self_cache_impl) = cache_and_impl(glace, &enum_name, asset_impl.is_some());

    let tokens = quote!(
        pub mod #mod_name {
            #edres_attrs

            use super::*;

            use std::{
                borrow::Cow,
                path::Path,
                time::SystemTime,
            };

            use #glace::{VirtualAsset, BytesAsset, StrAsset};

            #edres_tokens

            #self_cache

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
                        .unwrap_or_else(|| Self::_Unknown(#glace::_internal::fetch_name_index(name)))
                }

                fn try_name(self) -> #glace::Result<Cow<'static, str>> {
                    Ok(match self {
                        Self::_Unknown(index) => Cow::Owned(#glace::_internal::fetch_name(index)?),
                        c => Cow::Borrowed(c.const_name()),
                    })
                }
            }

            impl #glace::serde::Serialize for #enum_name {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: #glace::serde::Serializer
                {
                    self.name().serialize(serializer)
                }
            }

            impl<'de> #glace::serde::Deserialize<'de> for #enum_name {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: #glace::serde::Deserializer<'de>
                {
                    let name: &'de str = <&'de str as #glace::serde::Deserialize<'de>>::deserialize::<D>(deserializer)?;
                    Ok(Self::from_name(name))
                }
            }

            #base_asset_impls

            #serde_asset_impl

            #asset_impl

            #self_cache_impl
        }
    );

    Ok((tokens, quote!(#parents::#mod_name::#enum_name)))
}

#[allow(unused_variables)]
fn generate_single_module(
    context: Arc<Context>,
    path: &Path,
    mod_name: &str,
    parents: TokenStream2,
) -> Result<(TokenStream2, TokenStream2)> {
    let glace = &context.alias;
    let struct_str_name = mod_name.to_case(Case::Pascal);
    let struct_name = ident(&struct_str_name);
    let mod_name = ident(mod_name);
    let file_path = path.to_string_lossy();

    let main_type = MainType::of(&[path]);
    let asset_type = main_type.asset_type();

    let const_data: Option<TokenStream2> = cfg!(not(feature = "disable_const_data")).then(|| {
        quote!(
            const BYTES: &'static [u8] =
                include_bytes!(concat!(env!("CARGO_MANIFEST_DIR"), "/", #file_path));

            pub const fn const_bytes(self) -> &'static [u8] {
                Self::BYTES
            }

            pub const fn const_str(self) -> &'static str {
                unsafe { std::str::from_utf8_unchecked(self.const_bytes()) }
            }

            pub const fn try_const_str(self) -> Result<&'static str, std::str::Utf8Error> {
                unsafe { std::str::from_utf8(self.const_bytes()) }
            }
        )
    });

    let has_disk = cfg!(not(feature = "disable_disk_io"));
    let has_const = cfg!(not(feature = "disable_const_data"));

    let base_asset_impls = (has_const || has_disk).then(|| {
        let [known_try_bytes, known_try_string] = match has_const {
            true => [
                quote!(Cow::Borrowed(self.const_bytes())),
                quote!(Cow::Borrowed(self.try_const_str()?)),
            ],
            false => [
                quote!(Cow::Owned(self.load_bytes())),
                quote!(Cow::Owned(self.try_load_string()?)),
            ],
        };
        let [try_bytes_modified, try_load_bytes] =
            match has_disk {
                true => [
                    quote!(#glace::_internal::fetch_bytes_modified(Self::PATH.as_ref(), previous_modified)),
                    quote!(
                        Ok(std::fs::read(Self::PATH)?)
                    ),
                ],
                false => [
                    quote!(Ok(match previous_modified {
                        None => Some((self.try_bytes()?.into_owned(), SystemTime::UNIX_EPOCH)),
                        _ => None,
                    })),
                    quote!(Err(#glace::GlaceError::DiskIoDisabled)?),
                ],
            };

        quote!(
            impl BytesAsset for #struct_name {
                fn try_bytes(&self) -> #glace::Result<Cow<'static, [u8]>> {
                    Ok(#known_try_bytes)
                }

                fn try_bytes_modified(&self, previous_modified: Option<SystemTime>) -> #glace::Result<Option<(Vec<u8>, SystemTime)>> {
                    #try_bytes_modified
                }

                fn try_load_bytes(&self) -> #glace::Result<Vec<u8>> {
                    #try_load_bytes
                }
            }

            impl StrAsset for #struct_name {
                fn try_string(&self) -> #glace::Result<Cow<'static, str>> {
                    Ok(#known_try_string)
                }
            }
        )
    });

    let (specific_type, edres_tokens): (Option<StringyTokens>, Option<TokenStream2>) = {
        let overridden = context.explicit_type(path);
        if overridden.is_some() {
            (overridden, None)
        } else {
            let source = std::fs::read_to_string(path)?;
            let value_str_name = format!("{}Value", struct_str_name);
            let value_name = ident(&value_str_name);
            let value = match main_type {
                #[cfg(feature = "edres_json")]
                MainType::Json => {
                    edres::parsing::json::parse_source(&source, &edres::ParseOptions::new())?
                }

                #[cfg(feature = "edres_toml")]
                MainType::Toml => {
                    edres::parsing::toml::parse_source(&source, &edres::ParseOptions::new())?
                }

                #[cfg(feature = "edres_yaml")]
                MainType::Yaml => {
                    edres::parsing::yaml::parse_source(&source, &edres::ParseOptions::new())?
                }

                x => Err(eyre!("Failed to parse the file at `{path}`, detected to be a file of type `{x:?}`\n(Have you enabled the relevant `edres_*` feature?)", path=path.display()))?,
            };

            #[cfg(any(feature = "edres_json", feature = "edres_toml", feature = "edres_yaml"))]
            let tokens = Some(edres::codegen::define_structs(
                &value.assume_struct()?,
                &value_str_name,
                Some(path),
                &edres::Options {
                    serde_support: edres::SerdeSupport::Yes,
                    structs: edres::StructOptions {
                        derived_traits: vec!["Debug".into(), "Clone".into(), "PartialEq".into()]
                            .into(),
                        ..edres::StructOptions::minimal()
                    },
                    ..edres::Options::minimal()
                },
            )?);
            #[cfg(not(any(
                feature = "edres_json",
                feature = "edres_toml",
                feature = "edres_yaml"
            )))]
            let tokens = None;

            let specific_type = StringyTokens::from(Type::Verbatim(quote!(#value_name)));

            (Some(specific_type), tokens)
        }
    };
    let specific_type: Option<&StringyTokens> = specific_type.as_ref();
    let edres_attrs = edres_tokens
        .is_some()
        .then(|| quote!(#![allow(clippy::derive_partial_eq_without_eq)]));

    let serde_asset_impl = serde_asset_impl(glace, &struct_name, asset_type);
    let asset_impl = asset_impl(glace, &struct_name, asset_type, specific_type);
    let (self_cache, self_cache_impl) = cache_and_impl(glace, &struct_name, asset_impl.is_some());

    let tokens = quote!(
        pub mod #mod_name {
            #edres_attrs

            use super::*;

            use std::{
                borrow::Cow,
                path::Path,
                time::SystemTime,
            };

            use #glace::{SingleAsset, BytesAsset, StrAsset};

            #edres_tokens

            #self_cache

            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, #glace::serde::Serialize, #glace::serde::Deserialize)]
            pub struct #struct_name;

            impl #struct_name {
                #const_data

                // TODO(disk_io)
                // pub fn all_variants() -> impl Iterator<Item=Self> {
                //     todo!("read keys from actual file")
                // }
            }

            impl SingleAsset for #struct_name {
                const NAME: &'static str = #struct_str_name;
                const PATH: &'static str = #file_path;
            }

            #base_asset_impls

            #serde_asset_impl

            #asset_impl

            #self_cache_impl
        }
    );

    Ok((tokens, quote!(#parents::#mod_name::#struct_name)))
}

#[proc_macro]
pub fn glace(stream: TokenStream) -> TokenStream {
    use crate::compat_tests::*;
    use std::mem::transmute;

    let crate_dir = std::env::var_os("CARGO_MANIFEST_DIR");
    if let Some(path) = crate_dir {
        std::env::set_current_dir(path).unwrap();
    }

    unsafe {
        size_test::<SizeTest>();
        discriminant_test(transmute(LayoutTest::Zero), 0);
        discriminant_test(transmute(LayoutTest::One), 1);
        discriminant_test(transmute(LayoutTest::Other(123)), 2);
        field_test(transmute(LayoutTest::Other(123)), 123);
    }

    let call: GlaceMacro = parse_macro_input!(stream as GlaceMacro);
    let context = Arc::new(Context::from(call));

    generate_toplevel_module(
        Arc::clone(&context),
        &context.root_path,
        &context.root_mod_name,
    )
    .into()
}
