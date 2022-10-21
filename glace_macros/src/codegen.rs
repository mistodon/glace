use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{Ident, Type};

use crate::parsing::{GlaceMacro, Property};

#[derive(Debug, Clone)]
pub struct StringyTokens(String);

impl From<Ident> for StringyTokens {
    fn from(i: Ident) -> StringyTokens {
        StringyTokens(quote!(#i).to_string())
    }
}

impl From<Type> for StringyTokens {
    fn from(t: Type) -> StringyTokens {
        StringyTokens(quote!(#t).to_string())
    }
}

impl ToTokens for StringyTokens {
    fn to_tokens(&self, stream: &mut TokenStream) {
        let token_stream: TokenStream = self.0.parse().unwrap();
        token_stream.to_tokens(stream);
    }
}

pub fn ident(x: &str) -> Ident {
    quote::format_ident!("{}", x)
}

#[derive(Debug, Clone, Copy)]
pub enum ModType {
    Dir,
    Single,
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

    Unknown,
}

#[derive(Debug, Clone)]
pub enum AssetType {
    #[cfg(feature = "image")]
    Image,

    #[cfg(feature = "serde")]
    Serde(StringyTokens),

    #[cfg(feature = "serde")]
    EdresGenerated,

    String,
    Binary,
    None,
}

pub struct Config {
    pub const_data: bool,
    pub disk_io: bool,
    pub self_cached: bool,
    pub mod_type: ModType,
    pub explicit_file_type: Option<FileType>,
    pub explicit_asset_type: Option<AssetType>,
}

pub struct Context {
    pub alias: StringyTokens,
    pub root_path: PathBuf,
    pub root_mod_name: String,
    pub const_data: bool,
    pub disk_io: bool,
    pub self_cached: bool,
    pub overrides: HashMap<PathBuf, Config>,
}

impl Context {
    pub fn is_single(&self, path: &Path) -> bool {
        self.overrides
            .get(path)
            .map(|config| matches!(config.mod_type, ModType::Single))
            .unwrap_or(false)
    }

    pub fn is_virtual(&self, path: &Path) -> bool {
        self.overrides
            .get(path)
            .map(|config| matches!(config.mod_type, ModType::Virtual))
            .unwrap_or(false)
    }

    pub fn explicit_type(&self, path: &Path) -> Option<StringyTokens> {
        self.overrides
            .get(path)
            .and_then(|config| match &config.explicit_asset_type {
                Some(AssetType::Serde(ty)) => Some(ty.clone()),
                _ => None,
            })
    }
}

impl From<GlaceMacro> for Context {
    fn from(call: GlaceMacro) -> Context {
        let const_data = cfg!(not(feature = "disable_const_data"));
        let disk_io = cfg!(not(feature = "disable_disk_io"));
        let self_cached = cfg!(feature = "self_cached");

        let mut overrides = HashMap::new();

        for (path, props) in call.overrides.into_iter() {
            let mut config = Config {
                const_data,
                disk_io,
                self_cached,
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
                    Property::Single => config.mod_type = ModType::Single,
                    Property::Virtual => config.mod_type = ModType::Virtual,

                    #[cfg(feature = "serde")]
                    Property::Serde(ty) => {
                        config.explicit_asset_type =
                            Some(AssetType::Serde(StringyTokens::from(*ty)))
                    }
                }
            }

            overrides.insert(path, config);
        }

        Context {
            alias: StringyTokens::from(call.alias),
            root_path: call.path,
            root_mod_name: call.mod_name,
            const_data,
            disk_io,
            self_cached,
            overrides,
        }
    }
}
