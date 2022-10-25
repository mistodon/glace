use std::{
    collections::{HashMap, VecDeque},
    ffi::OsStr,
    path::{Path, PathBuf},
    sync::Arc,
};

use color_eyre::eyre::{eyre, Result};
use convert_case::{Case, Casing};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Ident, Type};

use crate::parsing::{GlaceMacro, ParseType, Property};

#[derive(Debug, Clone)]
pub struct StringyTokens(String);

impl From<&Ident> for StringyTokens {
    fn from(i: &Ident) -> StringyTokens {
        StringyTokens(quote!(#i).to_string())
    }
}

impl From<&Type> for StringyTokens {
    fn from(t: &Type) -> StringyTokens {
        StringyTokens(quote!(#t).to_string())
    }
}

impl From<&syn::Path> for StringyTokens {
    fn from(t: &syn::Path) -> StringyTokens {
        StringyTokens(quote!(#t).to_string())
    }
}

impl From<&syn::Visibility> for StringyTokens {
    fn from(t: &syn::Visibility) -> StringyTokens {
        StringyTokens(quote!(#t).to_string())
    }
}

impl ToTokens for StringyTokens {
    fn to_tokens(&self, stream: &mut TokenStream) {
        let token_stream: TokenStream = self.0.parse().unwrap();
        token_stream.to_tokens(stream);
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

#[derive(Debug, Clone, Copy)]
pub enum ModType {
    Dir,

    Ignore,

    #[cfg(feature = "serde")]
    Single,

    #[cfg(feature = "serde")]
    Virtual,
}

#[derive(Debug, Clone, Copy)]
pub enum FileType {
    #[cfg(feature = "image")]
    Image,

    #[cfg(feature = "serde_json")]
    Json,

    #[cfg(feature = "serde_toml")]
    Toml,

    #[cfg(feature = "serde_yaml")]
    Yaml,

    Raw,
}

#[derive(Debug, Clone)]
pub enum AssetType {
    #[cfg(feature = "image")]
    Image,

    #[cfg(feature = "serde")]
    Serde(StringyTokens),

    #[cfg(feature = "edres")]
    EdresGenerated,

    String,
    Binary,

    None,
}

#[derive(Clone)]
pub struct Config {
    pub const_data: bool,
    pub disk_io: bool,
    pub self_cached: bool,
    pub impl_serde: bool,
    pub allow_edres: bool,
    pub mod_type: ModType,
    pub explicit_file_type: Option<FileType>,
    pub explicit_asset_type: Option<AssetType>,
}

impl Config {
    fn _match_ext(files: &[impl AsRef<Path>], exts: &[&str]) -> bool {
        !files.is_empty()
            && files.iter().all(|path| {
                exts.iter()
                    .any(|ext| Some(ext.as_ref()) == path.as_ref().extension())
            })
    }

    pub fn file_type<P: AsRef<Path>>(&self, _paths: &[P]) -> FileType {
        self.explicit_file_type.unwrap_or_else(|| {
            #[cfg(feature = "image")]
            {
                if !_paths.is_empty()
                    && _paths
                        .iter()
                        .all(|p| image::ImageFormat::from_path(p).is_ok())
                {
                    return FileType::Image;
                }
            }
            #[cfg(feature = "serde_json")]
            {
                if Self::_match_ext(_paths, &["json"]) {
                    return FileType::Json;
                }
            }
            #[cfg(feature = "serde_toml")]
            {
                if Self::_match_ext(_paths, &["toml"]) {
                    return FileType::Toml;
                }
            }
            #[cfg(feature = "serde_yaml")]
            {
                if Self::_match_ext(_paths, &["yaml", "yml"]) {
                    return FileType::Yaml;
                }
            }
            FileType::Raw
        })
    }

    pub fn asset_type(&self, file_type: FileType) -> AssetType {
        self.explicit_asset_type.clone().unwrap_or(match file_type {
            #[cfg(feature = "image")]
            FileType::Image => AssetType::Image,

            #[cfg(feature = "edres_json")]
            FileType::Json if self.allow_edres => AssetType::EdresGenerated,

            #[cfg(feature = "edres_toml")]
            FileType::Toml if self.allow_edres => AssetType::EdresGenerated,

            #[cfg(feature = "edres_yaml")]
            FileType::Yaml if self.allow_edres => AssetType::EdresGenerated,

            _ => AssetType::Binary,
        })
    }
}

pub struct Context {
    pub alias: StringyTokens,
    pub root_visibility: Option<StringyTokens>,
    pub root_path: PathBuf,
    pub root_mod_name: String,
    pub const_data: bool,
    pub disk_io: bool,
    pub self_cached: bool,
    pub impl_serde: bool,
    pub allow_edres: bool,
    pub overrides: HashMap<PathBuf, Config>,
}

impl Context {
    pub fn config(&self, path: &Path) -> Config {
        self.overrides.get(path).cloned().unwrap_or(Config {
            const_data: self.const_data,
            disk_io: self.disk_io,
            self_cached: self.self_cached,
            impl_serde: self.impl_serde,
            allow_edres: self.allow_edres,
            mod_type: ModType::Dir,
            explicit_file_type: None,
            explicit_asset_type: None,
        })
    }

    pub fn is_normal(&self, path: &Path) -> bool {
        self.overrides.get(path).is_none()
    }

    pub fn is_ignored(&self, path: &Path) -> bool {
        self.overrides
            .get(path)
            .map(|config| matches!(config.mod_type, ModType::Ignore))
            .unwrap_or(false)
    }

    pub fn is_single(&self, _path: &Path) -> bool {
        #[cfg(feature = "serde")]
        {
            self.overrides
                .get(_path)
                .map(|config| matches!(config.mod_type, ModType::Single))
                .unwrap_or(false)
        }

        #[cfg(not(feature = "serde"))]
        {
            false
        }
    }

    pub fn is_virtual(&self, _path: &Path) -> bool {
        #[cfg(feature = "serde")]
        {
            self.overrides
                .get(_path)
                .map(|config| matches!(config.mod_type, ModType::Virtual))
                .unwrap_or(false)
        }

        #[cfg(not(feature = "serde"))]
        {
            false
        }
    }
}

impl From<GlaceMacro> for Context {
    fn from(call: GlaceMacro) -> Context {
        let const_data = cfg!(feature = "const_data");
        let disk_io = cfg!(feature = "disk_io");
        let self_cached = cfg!(feature = "self_cached");
        let impl_serde = cfg!(feature = "serde");
        let allow_edres = cfg!(feature = "edres");

        let mut overrides = HashMap::new();

        for (path, props) in call.overrides.into_iter() {
            let mut config = Config {
                const_data,
                disk_io,
                self_cached,
                impl_serde,
                allow_edres,
                mod_type: ModType::Dir,
                explicit_file_type: None,
                explicit_asset_type: None,
            };

            for prop in props {
                match prop {
                    Property::WithCache => config.self_cached = true,
                    Property::NoCache => config.self_cached = false,
                    Property::WithConst => config.const_data = true,
                    Property::NoConst => config.const_data = false,
                    Property::WithIO => config.disk_io = true,
                    Property::NoIO => config.disk_io = false,

                    Property::Ignore => config.mod_type = ModType::Ignore,

                    #[cfg(feature = "serde")]
                    Property::Single => config.mod_type = ModType::Single,

                    #[cfg(feature = "serde")]
                    Property::Virtual => config.mod_type = ModType::Virtual,

                    #[cfg(feature = "serde")]
                    Property::WithSerde => config.impl_serde = true,

                    #[cfg(feature = "serde")]
                    Property::NoSerde => config.impl_serde = false,

                    #[cfg(feature = "edres")]
                    Property::WithEdres => config.allow_edres = true,

                    #[cfg(feature = "edres")]
                    Property::NoEdres => config.allow_edres = false,

                    Property::Type(ty) => match ty {
                        #[cfg(feature = "image")]
                        ParseType::Image => {
                            config.explicit_file_type = Some(FileType::Image);
                            config.explicit_asset_type = Some(AssetType::Image);
                        }

                        #[cfg(feature = "serde_json")]
                        ParseType::Json => config.explicit_file_type = Some(FileType::Json),

                        #[cfg(feature = "serde_toml")]
                        ParseType::Toml => config.explicit_file_type = Some(FileType::Toml),

                        #[cfg(feature = "serde_yaml")]
                        ParseType::Yaml => config.explicit_file_type = Some(FileType::Yaml),

                        ParseType::String => {
                            config.explicit_file_type = Some(FileType::Raw);
                            config.explicit_asset_type = Some(AssetType::String);
                        }
                        ParseType::Binary => {
                            config.explicit_file_type = Some(FileType::Raw);
                            config.explicit_asset_type = Some(AssetType::Binary);
                        }
                        ParseType::None => {
                            config.explicit_asset_type = Some(AssetType::None);
                        }
                    },

                    #[cfg(feature = "serde")]
                    Property::Serde(ty) => {
                        config.explicit_asset_type =
                            Some(AssetType::Serde(StringyTokens::from(&*ty)))
                    }
                }
            }

            overrides.insert(path, config);
        }

        Context {
            alias: call
                .alias
                .as_ref()
                .map(StringyTokens::from)
                .unwrap_or_else(|| StringyTokens::from(&ident("glace"))),
            root_visibility: call.visibility.as_ref().map(StringyTokens::from),
            root_path: call.path,
            root_mod_name: call.mod_name,
            const_data,
            disk_io,
            self_cached,
            impl_serde,
            allow_edres,
            overrides,
        }
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

struct ModSpec {
    path: PathBuf,
    name: String,
    config: Config,
    dependencies: Vec<PathBuf>,
}

struct BuiltMod {
    _path: PathBuf,
    tokens: TokenStream,
    includes: TokenStream,
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
    _config: &Config,
) -> Option<TokenStream> {
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
        AssetType::Serde(ty) if _config.impl_serde => Some(quote!(
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
        let self_cache = quote!(
            #glace::lazy_static::lazy_static! {
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
            true => quote!(pub use #glace::{Asset, CachedAsset, SerdeAsset};),
            false => quote!(pub use #glace::{Asset, CachedAsset};),
        };
        quote!(
            pub mod prelude {
                #trait_exports
                #(#prelude_contents)*
            }
        )
    });

    let item_name = item_name(&spec.name);

    let item_tokens = gen_mod_contents(Arc::clone(&context), &spec, &item_name)?;

    let last = ident(path_name(spec.path.file_stem().unwrap()));
    let includes = match spec.path.strip_prefix(&context.root_path)?.parent() {
        Some(parent) => {
            let components = parent.components().filter_map(|component| match component {
                std::path::Component::Normal(component) => Some(ident(path_name(component))),
                _ => None,
            });

            let item_name = ident(&item_name);
            quote!(pub use super::#(#components::)*#last::#item_name;)
        }
        None => {
            quote!(pub use super::#last::#item_name;)
        }
    };

    let (vis, mod_name) = if is_root {
        (
            match &context.root_visibility {
                Some(vis) => quote!(#vis),
                None => quote!(),
            },
            ident(&context.root_mod_name),
        )
    } else {
        (quote!(pub), ident(&spec.name))
    };
    let sub_modules = spec.dependencies.iter().map(|path| &built[path].tokens);

    Ok(BuiltMod {
        _path: spec.path.clone(),
        tokens: quote!(
            #vis mod #mod_name {
                #![allow(clippy::derive_partial_eq_without_eq)]

                #prelude

                use super::*;

                #item_tokens

                #(#sub_modules)*
            }
        ),
        includes,
    })
}

fn gen_mod_contents(context: Arc<Context>, spec: &ModSpec, item_name: &str) -> Result<TokenStream> {
    Ok(match spec.config.mod_type {
        ModType::Dir => gen_dir_mod(context, spec, item_name)?,

        #[cfg(feature = "serde")]
        ModType::Single => gen_single_mod(context, spec, item_name)?,

        #[cfg(feature = "serde")]
        ModType::Virtual => gen_virtual_mod(context, spec, item_name)?,

        ModType::Ignore => unreachable!(),
    })
}

fn gen_dir_mod(context: Arc<Context>, spec: &ModSpec, item_name: &str) -> Result<TokenStream> {
    let glace = &context.alias;
    let enum_name = ident(&item_name);

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
    let variant_names = &variant_names;

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
    ));

    Ok(quote!(
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

        #serde_impl

        #base_asset_impls

        #serde_asset_impl

        #asset_impl

        #self_cache_impl
    ))
}

#[cfg(feature = "serde")]
fn gen_single_mod(context: Arc<Context>, spec: &ModSpec, item_name: &str) -> Result<TokenStream> {
    let glace = &context.alias;
    let struct_name = ident(&item_name);
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

    Ok(quote!(
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
    ))
}

#[cfg(feature = "serde")]
fn gen_virtual_mod(context: Arc<Context>, spec: &ModSpec, item_name: &str) -> Result<TokenStream> {
    let glace = &context.alias;
    let enum_name = ident(&item_name);
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
    let variant_str_names = data.keys().collect::<Vec<_>>();
    let variant_names = &variant_names;

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

    Ok(quote!(
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
    ))
}
