use std::{
    collections::{HashMap, VecDeque},
    ffi::OsStr,
    path::PathBuf,
    sync::Arc,
};

use color_eyre::eyre::{eyre, Result};
use convert_case::{Case, Casing};
use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

use crate::config::{AssetType, Config, Context, FileType, ModType, StringyTokens};

fn docs(_tokens: TokenStream) -> Option<TokenStream> {
    #[cfg(feature = "example_documentation")]
    {
        Some(_tokens)
    }

    #[cfg(not(feature = "example_documentation"))]
    {
        None
    }
}

pub fn ident<S: AsRef<str>>(x: S) -> Ident {
    quote::format_ident!("{}", x.as_ref())
}

pub fn path_name(x: &OsStr) -> String {
    x.to_string_lossy().to_case(Case::Snake)
}

pub fn item_name(x: &str) -> String {
    x.to_case(Case::Pascal)
}

struct ModSpec {
    path: PathBuf,
    name: String,
    config: Config,
    dependencies: Vec<PathBuf>,
}

struct BuiltMod {
    spec: ModSpec,
    tokens: TokenStream,
    includes: TokenStream,
    variants: Option<Vec<Ident>>,
}

struct BuiltModContents {
    tokens: TokenStream,
    variants: Option<Vec<Ident>>,
}

fn serde_asset_impl(
    _glace: &StringyTokens,
    _item_name: &Ident,
    file_type: FileType,
    config: &Config,
) -> Option<TokenStream> {
    match file_type {
        _ if !config.impl_serde => None,

        #[cfg(feature = "serde_json")]
        FileType::Json => Some(quote!(
            impl #_glace::SerdeAsset for #_item_name {
                fn deserialize<'b, T: for<'a> #_glace::serde::Deserialize<'a>>(bytes: Cow<'b, [u8]>) -> #_glace::Result<T> {
                    #_glace::_internal::load::load_json(bytes)
                }
            }
        )),

        #[cfg(feature = "serde_toml")]
        FileType::Toml => Some(quote!(
            impl #_glace::SerdeAsset for #_item_name {
                fn deserialize<'b, T: for<'a> #_glace::serde::Deserialize<'a>>(bytes: Cow<'b, [u8]>) -> #_glace::Result<T> {
                    #_glace::_internal::load::load_toml(bytes)
                }
            }
        )),

        #[cfg(feature = "serde_yaml")]
        FileType::Yaml => Some(quote!(
            impl #_glace::SerdeAsset for #_item_name {
                fn deserialize<'b, T: for<'a> #_glace::serde::Deserialize<'a>>(bytes: Cow<'b, [u8]>) -> #_glace::Result<T> {
                    #_glace::_internal::load::load_yaml(bytes)
                }
            }
        )),

        _ => None,
    }
}

fn asset_impl(
    glace: &StringyTokens,
    item_name: &Ident,
    asset_type: AssetType,
    config: &Config,
) -> Option<TokenStream> {
    if !(config.disk_io || config.const_data) {
        return None;
    }

    match asset_type {
        #[cfg(feature = "image")]
        AssetType::Image => Some(quote!(
            impl #glace::Asset for #item_name {
                type Value = #glace::image::RgbaImage;

                fn load(bytes: Cow<[u8]>) -> #glace::Result<Self::Value> {
                    #glace::_internal::load::load_image(bytes)
                }
            }
        )),

        #[cfg(feature = "serde")]
        AssetType::Serde(ty) if config.impl_serde => Some(quote!(
            impl #glace::Asset for #item_name {
                type Value = #ty;

                fn load(bytes: Cow<[u8]>) -> #glace::Result<Self::Value> {
                    <Self as #glace::SerdeAsset>::deserialize(bytes)
                }
            }
        )),

        #[cfg(feature = "edres")]
        AssetType::EdresGenerated => unreachable!("Type should have been generated already"),

        AssetType::String => Some(quote!(
            impl #glace::Asset for #item_name {
                type Value = Cow<'static, str>;

                fn load(bytes: Cow<'static, [u8]>) -> #glace::Result<Self::Value> {
                    match bytes {
                        Cow::Owned(bytes) => Cow::Owned(String::from_utf8(bytes)?),
                        Cow::Borrowed(bytes) => Cow::Borrowed(std::str::from_utf8(bytes)?),
                    }
                }
            }
        )),

        AssetType::None => None,

        _ => Some(quote!(
            impl #glace::Asset for #item_name {
                type Value = Cow<'static, [u8]>;

                fn load(bytes: Cow<'static, [u8]>) -> #glace::Result<Self::Value> {
                    Ok(bytes)
                }
            }
        )),
    }
}

fn cache_and_impl(
    glace: &StringyTokens,
    item_name: &Ident,
    is_asset: bool,
    config: &Config,
) -> (Option<TokenStream>, Option<TokenStream>) {
    if config.self_cached && is_asset {
        let doc = docs(quote!(
            /// TODO
        ));
        let self_cache = quote!(
            #glace::lazy_static::lazy_static! {
                #doc
                pub static ref CACHE: #glace::cache::RwCache<#item_name, <#item_name as #glace::Asset>::Value> = #glace::cache::RwCache::new();
            }
        );
        let cached_impl = quote!(
            impl #glace::CachedAsset for #item_name {
                type CacheType = #glace::cache::RwCache<Self, Self::Value>;

                fn cache() -> &'static Self::CacheType {
                    &CACHE
                }
            }
        );
        (Some(self_cache), Some(cached_impl))
    } else {
        (None, None)
    }
}

pub fn generate_all_modules(context: Arc<Context>) -> Result<TokenStream> {
    let dir_context = Arc::clone(&context);
    let spec_context = Arc::clone(&context);

    let mut working_set = VecDeque::new();

    ignore::WalkBuilder::new(&context.root_path)
        .sort_by_file_name(std::ffi::OsStr::cmp)
        .filter_entry(move |entry| {
            entry
                .file_type()
                .map(|f| {
                    f.is_dir() && !dir_context.is_ignored(entry.path())
                        || dir_context.is_single(entry.path())
                        || dir_context.is_virtual(entry.path())
                })
                .unwrap_or(false)
        })
        .build()
        .filter_map(|entry| entry.ok())
        .for_each(|entry| {
            let path = entry.into_path();
            let name = path_name(path.file_stem().unwrap());
            let config = spec_context.config(&path);

            let spec = ModSpec {
                path,
                name,
                config,
                dependencies: vec![],
            };

            working_set.push_front(spec);
        });

    // Work out dependencies
    {
        for i in 0..working_set.len() {
            let parent = working_set[i].path.clone();
            let mut dependencies = vec![];
            for spec in &working_set {
                if spec.path.parent() == Some(&parent) {
                    dependencies.push(spec.path.clone());
                }
            }
            dependencies.sort();
            working_set[i].dependencies = dependencies;
        }
    }

    let mut built = HashMap::new();

    let mut cycle_counter = working_set.len();
    while let Some(spec) = working_set.pop_front() {
        let all_dependencies_met = spec
            .dependencies
            .iter()
            .all(|path| built.contains_key(path));

        if all_dependencies_met {
            let path = spec.path.clone();
            let result = gen_mod_new(Arc::clone(&context), spec, &built)?;
            built.insert(path, result);
            cycle_counter = working_set.len();
        } else {
            working_set.push_back(spec);
            cycle_counter -= 1;
            if cycle_counter == 0 {
                return Err(eyre!("Cyclic dependency chain detected"));
            }
        }
    }

    Ok(built.remove(&context.root_path).unwrap().tokens)
}

fn gen_mirrors(built: &HashMap<PathBuf, BuiltMod>) -> Result<TokenStream> {
    let mut impls = vec![];

    for module in built.values() {
        for mirror_path in &module.spec.config.mirrors {
            let target = &built[mirror_path];
            let from_type = &module.includes;
            let to_type = &target.includes;

            if module.variants != target.variants {
                return Err(eyre!("Cannot mirror between types with different variants"));
            }

            impls.push(quote!(
                impl From<#from_type> for #to_type {
                    fn from(x: #from_type) -> Self {
                        match x {
                            #from_type::_Unknown(i) => todo!(),
                            _ => unsafe { std::mem::transmute(x) }
                        }
                    }
                }

                impl From<#to_type> for #from_type {
                    fn from(x: #to_type) -> Self {
                        match x {
                            #to_type::_Unknown(i) => todo!(),
                            _ => unsafe { std::mem::transmute(x) }
                        }
                    }
                }
            ));
        }
    }

    Ok(quote!(#(#impls)*))
}

fn gen_mod_new(
    context: Arc<Context>,
    spec: ModSpec,
    built: &HashMap<PathBuf, BuiltMod>,
) -> Result<BuiltMod> {
    let is_root = spec.path == context.root_path;
    let prelude = is_root.then(|| {
        let prelude_contents = built.values().map(|m| &m.includes);
        let glace = &context.alias;
        let trait_exports = match cfg!(feature = "serde") {
            true => quote!(pub use #glace::{Asset, CachedAsset, FileAsset, SerdeAsset, SingleAsset, VirtualAsset};),
            false => quote!(pub use #glace::{Asset, CachedAsset, FileAsset, SingleAsset, VirtualAsset};),
        };
        quote!(
            pub mod prelude {
                #trait_exports
                #(pub use super::#prelude_contents;)*
            }
        )
    });

    let module_docs = docs(match is_root {
        true => quote!(
            //! This is the root module.
        ),
        false => quote!(
            //! This is a sub module.
        ),
    });

    let item_name = item_name(&spec.name);
    let contents = gen_mod_contents(Arc::clone(&context), &spec, &item_name)?;
    let item_name = ident(item_name);

    let BuiltModContents {
        tokens: item_tokens,
        variants,
    } = contents;

    let last = ident(path_name(spec.path.file_stem().unwrap()));
    let includes = match spec.path.strip_prefix(&context.root_path)?.parent() {
        Some(parent) => {
            let components = parent.components().filter_map(|component| match component {
                std::path::Component::Normal(component) => Some(ident(path_name(component))),
                _ => None,
            });

            quote!(#(#components::)*#last::#item_name)
        }
        None => {
            quote!(#last::#item_name)
        }
    };

    let (vis, mod_name, mirrors) = if is_root {
        (
            match &context.root_visibility {
                Some(vis) => quote!(#vis),
                None => quote!(),
            },
            ident(&context.root_mod_name),
            Some(gen_mirrors(built)?),
        )
    } else {
        (quote!(pub), ident(&spec.name), None)
    };
    let sub_modules = spec
        .dependencies
        .iter()
        .map(|path| &built[path].tokens)
        .collect::<Vec<_>>();

    Ok(BuiltMod {
        spec,
        tokens: quote!(
            #vis mod #mod_name {
                #module_docs

                #![allow(clippy::derive_partial_eq_without_eq)]

                #prelude

                use super::*;

                #item_tokens

                #mirrors

                #(#sub_modules)*
            }
        ),
        includes,
        variants,
    })
}

fn gen_mod_contents(
    context: Arc<Context>,
    spec: &ModSpec,
    item_name: &str,
) -> Result<BuiltModContents> {
    Ok(match spec.config.mod_type {
        ModType::Dir => gen_dir_mod(context, spec, item_name)?,

        #[cfg(feature = "serde")]
        ModType::Single => gen_single_mod(context, spec, item_name)?,

        #[cfg(feature = "serde")]
        ModType::Virtual => gen_virtual_mod(context, spec, item_name)?,

        ModType::Ignore => unreachable!(),
    })
}

fn gen_dir_mod(context: Arc<Context>, spec: &ModSpec, item_name: &str) -> Result<BuiltModContents> {
    let glace = &context.alias;
    let enum_name = ident(item_name);

    let file_context = Arc::clone(&context);

    let files = ignore::WalkBuilder::new(&spec.path)
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
            is_file && !skipped && file_context.is_normal(entry.path())
        })
        .build()
        .skip(1)
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.into_path())
        .collect::<Vec<_>>();

    let dir_path = spec.path.to_string_lossy();

    let variant_str_names = files
        .iter()
        .map(|filepath| {
            let filepath = filepath
                .strip_prefix(&spec.path)
                .unwrap()
                .file_stem()
                .unwrap();
            let variant_name = filepath.to_string_lossy().as_ref().to_case(Case::Pascal);
            variant_name
        })
        .collect::<Vec<_>>();
    let variant_names = variant_str_names.iter().map(ident).collect::<Vec<_>>();
    let variant_names_ref = &variant_names;

    let variant_paths = files
        .iter()
        .map(|filepath| filepath.to_string_lossy())
        .collect::<Vec<_>>();
    let variant_paths = &variant_paths;

    let has_disk = spec.config.disk_io;
    let has_const = spec.config.const_data;

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

    let file_type = spec.config.file_type(&files);
    let serde_asset_impl = serde_asset_impl(glace, &enum_name, file_type, &spec.config);

    let asset_type = spec.config.asset_type(file_type);
    let (asset_type, edres_tokens): (AssetType, Option<TokenStream>) = match asset_type {
        #[cfg(feature = "edres")]
        AssetType::EdresGenerated => {
            let value_str_name = format!("{}Value", item_name);
            let value_name = ident(&value_str_name);
            let format: edres::Format = match file_type {
                    #[cfg(feature = "edres_json")]
                    FileType::Json => edres::Format::Json,

                    #[cfg(feature = "edres_toml")]
                    FileType::Toml => edres::Format::Toml,

                    #[cfg(feature = "edres_yaml")]
                    FileType::Yaml => edres::Format::Yaml,

                    _ => Err(eyre!("edres cannot generate types for {:?} files with the currently enabled features", file_type))?,
                };

            let tokens = Some(edres::codegen::define_structs_from_file_contents(
                &spec.path,
                &value_str_name,
                Some(format),
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

            (AssetType::Serde(StringyTokens::from(&value_name)), tokens)
        }
        other => (other, None),
    };

    let asset_impl = asset_impl(glace, &enum_name, asset_type, &spec.config);

    let (self_cache, self_cache_impl) =
        cache_and_impl(glace, &enum_name, asset_impl.is_some(), &spec.config);

    let const_data: Option<TokenStream> = spec.config.const_data.then(|| {
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

    let serde_impl = spec.config.impl_serde.then(|| quote!(
            impl #glace::serde::Serialize for #enum_name {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: #glace::serde::Serializer
                {
                    match *self {
                        Self::_Unknown(_) => {
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
    ));

    Ok(BuiltModContents {
        tokens: quote!(
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
                #(#variant_names_ref,)*
                _Unknown(u32),
            }

            impl #enum_name {
                pub const ALL: &'static [Self] = &[
                    #(Self::#variant_names_ref,)*
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
                    #glace::_internal::visit_files(Self::PARENT.as_ref(), Self::from_path_internal)
                }

                fn from_path_internal(path: &Path) -> Self {
                    Self::from_path(path)
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

                fn from_const_path<P: AsRef<Path>>(path: P) -> Option<Self> {
                    let path = path.as_ref();
                    Self::PATHS.iter()
                        .position(|&p| path == <str as AsRef<Path>>::as_ref(p))
                        .map(|index| Self::ALL[index])
                }

                fn from_const_name(name: &str) -> Option<Self> {
                    Self::NAMES.iter()
                        .position(|&n| n == name)
                        .map(|index| Self::ALL[index])
                }

                fn from_path_unchecked<P: AsRef<Path>>(path: P) -> Self {
                    let path = path.as_ref();
                    Self::from_const_path(path)
                        .unwrap_or_else(|| Self::_Unknown(#glace::_internal::fetch_path_index(path)))
                }

                fn try_path(self) -> #glace::Result<Cow<'static, Path>> {
                    Ok(match self {
                        Self::_Unknown(index) => Cow::Owned(#glace::_internal::fetch_path(index)?),
                        c => Cow::Borrowed(c.const_path().as_ref()),
                    })
                }
            }

            #serde_impl

            #base_asset_impls

            #serde_asset_impl

            #asset_impl

            #self_cache_impl
        ),
        variants: Some(variant_names),
    })
}

#[cfg(feature = "serde")]
fn gen_single_mod(
    context: Arc<Context>,
    spec: &ModSpec,
    item_name: &str,
) -> Result<BuiltModContents> {
    let glace = &context.alias;
    let struct_name = ident(item_name);
    let file_path = spec.path.to_string_lossy();

    let file_type = spec.config.file_type(&[&spec.path]);

    let const_data: Option<TokenStream> = spec.config.const_data.then(|| {
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

    let has_disk = spec.config.disk_io;
    let has_const = spec.config.const_data;

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

    let serde_asset_impl = serde_asset_impl(glace, &struct_name, file_type, &spec.config);

    let asset_type = spec.config.asset_type(file_type);
    let (asset_type, edres_tokens): (AssetType, Option<TokenStream>) = match asset_type {
        #[cfg(feature = "edres")]
        AssetType::EdresGenerated => {
            let source = std::fs::read_to_string(&spec.path)?;
            let value_str_name = format!("{}Value", item_name);
            let value_name = ident(&value_str_name);
            let value = match file_type {
                    #[cfg(feature = "edres_json")]
                    FileType::Json => {
                        edres::parsing::json::parse_source(&source, &edres::ParseOptions::new())?
                    }

                    #[cfg(feature = "edres_toml")]
                    FileType::Toml => {
                        edres::parsing::toml::parse_source(&source, &edres::ParseOptions::new())?
                    }

                    #[cfg(feature = "edres_yaml")]
                    FileType::Yaml => {
                        edres::parsing::yaml::parse_source(&source, &edres::ParseOptions::new())?
                    }

                    _ => Err(eyre!("edres cannot generate types for {:?} files with the currently enabled features", file_type))?,
                };

            let tokens = Some(edres::codegen::define_structs(
                &value.assume_struct()?,
                &value_str_name,
                Some(&spec.path),
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

            (AssetType::Serde(StringyTokens::from(&value_name)), tokens)
        }
        other => (other, None),
    };

    let asset_impl = asset_impl(glace, &struct_name, asset_type, &spec.config);
    let (self_cache, self_cache_impl) =
        cache_and_impl(glace, &struct_name, asset_impl.is_some(), &spec.config);

    let derives = match spec.config.impl_serde {
        true => {
            quote!(#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, #glace::serde::Serialize, #glace::serde::Deserialize)])
        }
        false => quote!(#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]),
    };

    Ok(BuiltModContents {
        tokens: quote!(
            use std::{
                borrow::Cow,
                path::Path,
                time::SystemTime,
            };

            use #glace::{SingleAsset, BytesAsset, StrAsset};

            #edres_tokens

            #self_cache

            #derives
            pub struct #struct_name;

            impl #struct_name {
                #const_data

                // TODO(disk_io)
                // pub fn all_variants() -> impl Iterator<Item=Self> {
                //     todo!("read keys from actual file")
                // }
            }

            impl SingleAsset for #struct_name {
                const NAME: &'static str = #item_name;
                const PATH: &'static str = #file_path;
            }

            #base_asset_impls

            #serde_asset_impl

            #asset_impl

            #self_cache_impl
        ),
        variants: None,
    })
}

#[cfg(feature = "serde")]
fn gen_virtual_mod(
    context: Arc<Context>,
    spec: &ModSpec,
    item_name: &str,
) -> Result<BuiltModContents> {
    let glace = &context.alias;
    let enum_name = ident(item_name);
    let file_path = spec.path.to_string_lossy();

    let file_type = spec.config.file_type(&[&spec.path]);

    let _source = std::fs::read_to_string(&spec.path).unwrap();
    let data: indexmap::IndexMap<String, std::borrow::Cow<[u8]>> = match file_type {
        #[cfg(feature = "serde_json")]
        FileType::Json => {
            let map: indexmap::IndexMap<String, &serde_json::value::RawValue> =
                serde_json::from_str(&_source).unwrap();
            map.into_iter()
                .map(|(k, v)| (k, std::borrow::Cow::Borrowed(v.get().as_bytes())))
                .collect()
        }

        #[cfg(feature = "serde_toml")]
        FileType::Toml => {
            let map: indexmap::IndexMap<String, toml::Value> = toml::from_str(&_source).unwrap();
            map.into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        std::borrow::Cow::Owned(toml::to_string(&v).unwrap().into()),
                    )
                })
                .collect()
        }

        #[cfg(feature = "serde_yaml")]
        FileType::Yaml => {
            let map: indexmap::IndexMap<String, serde_yaml::Value> =
                serde_yaml::from_str(&_source).unwrap();
            map.into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        std::borrow::Cow::Owned(serde_yaml::to_string(&v).unwrap().into()),
                    )
                })
                .collect()
        }
        _ => Err(eyre!(
            "Cannot load {:?} files with the currently enabled features",
            file_type
        ))?,
    };

    let variant_names = data.keys().map(ident).collect::<Vec<_>>();
    let variant_str_names = data.keys().cloned().collect::<Vec<_>>();
    let variant_names_ref = &variant_names;

    let value_bytes = data
        .values()
        .map(|bs| proc_macro2::Literal::byte_string(bs));

    let const_data: Option<TokenStream> = spec.config.const_data.then(|| {
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

    let try_load_all_fn: TokenStream = match file_type {
        #[cfg(feature = "serde_json")]
        FileType::Json => quote!(
            fn try_load_all(bytes: &[u8]) -> #glace::Result<#glace::indexmap::IndexMap<Self, Vec<u8>>> {
                #glace::_internal::load::load_all_json(bytes)
            }
        ),

        #[cfg(feature = "serde_toml")]
        FileType::Toml => quote!(
            fn try_load_all(bytes: &[u8]) -> #glace::Result<#glace::indexmap::IndexMap<Self, Vec<u8>>> {
                #glace::_internal::load::load_all_toml(bytes)
            }
        ),

        #[cfg(feature = "serde_yaml")]
        FileType::Yaml => quote!(
            fn try_load_all(bytes: &[u8]) -> #glace::Result<#glace::indexmap::IndexMap<Self, Vec<u8>>> {
                #glace::_internal::load::load_all_yaml(bytes)
            }
        ),
        _ => Err(eyre!(
            "Cannot load {:?} files with the currently enabled features",
            file_type
        ))?,
    };

    let has_disk = spec.config.disk_io;
    let has_const = spec.config.const_data;

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

    let serde_asset_impl = serde_asset_impl(glace, &enum_name, file_type, &spec.config);

    let asset_type = spec.config.asset_type(file_type);
    let (asset_type, edres_tokens): (AssetType, Option<TokenStream>) = match asset_type {
        #[cfg(feature = "edres")]
        AssetType::EdresGenerated => {
            let source = std::fs::read_to_string(&spec.path)?;
            let value_str_name = format!("{}Value", item_name);
            let value_name = ident(&value_str_name);
            let value = match file_type {
                #[cfg(feature = "edres_json")]
                FileType::Json => {
                    edres::parsing::json::parse_source(&source, &edres::ParseOptions::new())?
                }

                #[cfg(feature = "edres_toml")]
                FileType::Toml => {
                    edres::parsing::toml::parse_source(&source, &edres::ParseOptions::new())?
                }

                #[cfg(feature = "edres_yaml")]
                FileType::Yaml => {
                    edres::parsing::yaml::parse_source(&source, &edres::ParseOptions::new())?
                }

                _ => Err(eyre!("edres cannot generate types for {:?} files with the currently enabled features", file_type))?,
            };

            let tokens = Some(edres::codegen::define_structs_from_values(
                &value.assume_struct()?,
                &value_str_name,
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

            let specific_type = StringyTokens::from(&value_name);

            (AssetType::Serde(specific_type), tokens)
        }
        other => (other, None),
    };

    let asset_impl = asset_impl(glace, &enum_name, asset_type, &spec.config);

    let (self_cache, self_cache_impl) =
        cache_and_impl(glace, &enum_name, asset_impl.is_some(), &spec.config);

    let serde_impl = spec.config.impl_serde.then(|| quote!(
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
    ));

    Ok(BuiltModContents {
        tokens: quote!(
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
                #(#variant_names_ref,)*
                _Unknown(u32),
            }

            impl #enum_name {
                pub const ALL: &'static [Self] = &[
                    #(Self::#variant_names_ref,)*
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
                const PATH: &'static str = #file_path;

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

            #serde_impl

            #base_asset_impls

            #serde_asset_impl

            #asset_impl

            #self_cache_impl
        ),
        variants: Some(variant_names),
    })
}
