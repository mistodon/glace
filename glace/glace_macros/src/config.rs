use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Ident, Type};

use crate::{
    codegen::ident,
    parsing::{GlaceMacro, ParseType, Property},
};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SetBool {
    Inferred(bool),
    Explicit(bool),
}

#[derive(Clone)]
pub struct Config {
    pub const_data: bool,
    pub disk_io: bool,
    pub self_cached: SetBool,
    pub impl_serde: bool,
    pub allow_edres: bool,
    pub mod_type: ModType,
    pub explicit_file_type: Option<FileType>,
    pub explicit_asset_type: Option<AssetType>,
    pub mirrors: Vec<PathBuf>,
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
        self.explicit_file_type.unwrap_or({
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
    pub self_cached: SetBool,
    pub impl_serde: bool,
    pub allow_edres: bool,
    pub overrides: HashMap<PathBuf, Config>,
}

impl Context {
    pub fn config(&self, path: &Path) -> Config {
        self.overrides
            .get(self.fix_path(path))
            .cloned()
            .unwrap_or(Config {
                const_data: self.const_data,
                disk_io: self.disk_io,
                self_cached: self.self_cached,
                impl_serde: self.impl_serde,
                allow_edres: self.allow_edres,
                mod_type: ModType::Dir,
                explicit_file_type: None,
                explicit_asset_type: None,
                mirrors: vec![],
            })
    }

    fn fix_path<'a>(&self, path: &'a Path) -> &'a Path {
        path.strip_prefix(&self.root_path).unwrap()
    }

    pub fn is_normal(&self, path: &Path) -> bool {
        self.overrides.get(self.fix_path(path)).is_none()
    }

    pub fn is_ignored(&self, path: &Path) -> bool {
        self.overrides
            .get(self.fix_path(path))
            .map(|config| matches!(config.mod_type, ModType::Ignore))
            .unwrap_or(false)
    }

    pub fn is_single(&self, _path: &Path) -> bool {
        #[cfg(feature = "serde")]
        {
            self.overrides
                .get(self.fix_path(_path))
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
                .get(self.fix_path(_path))
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
        let self_cached = SetBool::Inferred(cfg!(feature = "self_cached"));
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
                mirrors: vec![],
            };

            for prop in props {
                match prop {
                    Property::WithCache => config.self_cached = SetBool::Explicit(true),
                    Property::NoCache => config.self_cached = SetBool::Explicit(false),
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

                    Property::Mirror(path) => {
                        let mut full_path = call.path.clone();
                        full_path.push(path);
                        config.mirrors.push(full_path);
                    }

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
