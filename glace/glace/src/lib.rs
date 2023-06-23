pub use glace_macros::glace;

#[cfg(feature = "image")]
pub use image;

pub use indexmap;

#[cfg(feature = "self_cached")]
pub use lazy_static;

#[cfg(feature = "serde")]
pub use serde;

pub mod _internal;
pub mod cache;

#[cfg(doc)]
pub mod _example_docs;

use std::{
    borrow::Cow,
    hash::Hash,
    path::{Path, PathBuf},
    sync::Arc,
    time::SystemTime,
};

use indexmap::IndexMap;
use thiserror::Error;

use crate::cache::Cache;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
pub enum PathedKey<'a> {
    Known(&'a str),
    Path { path: &'a Path },
}

#[derive(Debug, Error)]
pub enum GlaceError {
    #[error("Path `{0}` is not under the correct parent directory (`{1}`)")]
    MismatchedParent(PathBuf, PathBuf),

    #[error("No Path cached for the given index: `{0}`\nUse the `from_path` method instead of constructing a `_Unknown(_)` variant directly.")]
    NoPathCached(u32),

    #[error("No name cached for the given index: `{0}`\nUse the `from_name` method instead of constructing an `_Unknown(_)` variant directly.")]
    NoNameCached(u32),

    #[error("Disk IO is disabled so this load will always fail")]
    DiskIoDisabled,

    #[error("IO error")]
    Io(#[from] std::io::Error),

    #[error("UTF8 error")]
    Utf8(#[from] std::str::Utf8Error),

    #[error("UTF8 error")]
    FromUtf8(#[from] std::string::FromUtf8Error),
}

pub type Result<T> = std::result::Result<T, GlaceError>;

pub trait FileAsset: Sized {
    const PARENT: &'static str;

    fn from_const_name(name: &str) -> Option<Self>;
    fn from_const_path<P: AsRef<Path>>(path: P) -> Option<Self>;
    fn from_path_unchecked<P: AsRef<Path>>(path: P) -> Self;
    fn try_path(self) -> Result<Cow<'static, Path>>;

    fn from_path<P: AsRef<Path>>(path: P) -> Self {
        Self::try_from_path(path).unwrap()
    }

    fn try_from_path<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = path.as_ref();
        _internal::verify_parent(path, Self::PARENT)?;
        Ok(Self::from_path_unchecked(path))
    }

    fn path(self) -> Cow<'static, Path> {
        self.try_path().unwrap()
    }
}

pub trait VirtualAsset: Sized {
    const PATH: &'static str;

    fn try_load_all(bytes: &[u8]) -> Result<IndexMap<Self, Vec<u8>>>;

    fn from_const_name(name: &str) -> Option<Self>;
    fn from_name(name: &str) -> Self;
    fn try_name(self) -> Result<Cow<'static, str>>;

    fn name(self) -> Cow<'static, str> {
        self.try_name().unwrap()
    }
}

pub trait SingleAsset: Sized {
    const NAME: &'static str;
    const PATH: &'static str;
}

pub trait BytesAsset {
    fn try_bytes(&self) -> Result<Cow<'static, [u8]>>;
    fn try_bytes_modified(
        &self,
        previous_modified: Option<SystemTime>,
    ) -> Result<Option<(Vec<u8>, SystemTime)>>;
    fn try_load_bytes(&self) -> Result<Vec<u8>>;

    fn bytes(&self) -> Cow<'static, [u8]> {
        self.try_bytes().unwrap()
    }

    fn bytes_modified(
        &self,
        previous_modified: Option<SystemTime>,
    ) -> Option<(Vec<u8>, SystemTime)> {
        self.try_bytes_modified(previous_modified).unwrap()
    }

    fn load_bytes(&self) -> Vec<u8> {
        self.try_load_bytes().unwrap()
    }

    fn bytes_fallback(&self) -> Cow<'static, [u8]> {
        match self.try_load_bytes() {
            Ok(x) => Cow::Owned(x),
            Err(_) => self.bytes(),
        }
    }

    fn try_bytes_fallback(&self) -> Result<Cow<'static, [u8]>> {
        match self.try_load_bytes() {
            Ok(x) => Ok(Cow::Owned(x)),
            Err(_) => self.try_bytes(),
        }
    }
}

pub trait StrAsset: BytesAsset {
    fn try_string(&self) -> Result<Cow<'static, str>>;

    fn try_string_modified(
        &self,
        previous_modified: Option<SystemTime>,
    ) -> Result<Option<(String, SystemTime)>> {
        Ok(match self.try_bytes_modified(previous_modified)? {
            Some((bytes, time)) => Some((String::from_utf8(bytes)?, time)),
            None => None,
        })
    }

    fn try_load_string(&self) -> Result<String> {
        Ok(String::from_utf8(self.try_load_bytes()?)?)
    }

    fn string(&self) -> Cow<'static, str> {
        self.try_string().unwrap()
    }

    fn string_modified(
        &self,
        previous_modified: Option<SystemTime>,
    ) -> Option<(String, SystemTime)> {
        self.try_string_modified(previous_modified).unwrap()
    }

    fn load_string(&self) -> String {
        self.try_load_string().unwrap()
    }

    fn string_fallback(&self) -> Cow<'static, str> {
        match self.try_load_string() {
            Ok(x) => Cow::Owned(x),
            Err(_) => self.string(),
        }
    }

    fn try_string_fallback(&self) -> Result<Cow<'static, str>> {
        match self.try_load_string() {
            Ok(x) => Ok(Cow::Owned(x)),
            Err(_) => self.try_string(),
        }
    }
}

pub trait Asset: BytesAsset {
    type Value;

    fn load(bytes: Cow<'static, [u8]>) -> Result<Self::Value>;

    fn try_value(&self) -> Result<Self::Value> {
        let bytes = self.try_bytes()?;
        Self::load(bytes)
    }

    fn try_value_modified(
        &self,
        previous_modified: Option<SystemTime>,
    ) -> Result<Option<(Self::Value, SystemTime)>> {
        Ok(match self.try_bytes_modified(previous_modified)? {
            Some((bytes, time)) => Some((Self::load(Cow::Owned(bytes))?, time)),
            None => None,
        })
    }

    fn try_load_value(&self) -> Result<Self::Value> {
        let bytes = self.try_load_bytes()?;
        Self::load(Cow::Owned(bytes))
    }

    fn value(&self) -> Self::Value {
        self.try_value().unwrap()
    }

    fn value_modified(
        &self,
        previous_modified: Option<SystemTime>,
    ) -> Option<(Self::Value, SystemTime)> {
        self.try_value_modified(previous_modified).unwrap()
    }

    fn load_value(&self) -> Self::Value {
        self.try_load_value().unwrap()
    }

    fn value_fallback(&self) -> Self::Value {
        self.try_load_value().unwrap_or_else(|_| self.value())
    }

    fn try_value_fallback(&self) -> Result<Self::Value> {
        self.try_load_value().or_else(|_| self.try_value())
    }
}

#[cfg(feature = "serde")]
pub trait SerdeAsset: BytesAsset {
    fn deserialize<'b, T: for<'a> serde::Deserialize<'a>>(bytes: Cow<'b, [u8]>) -> Result<T>;

    fn try_parsed_value<T: for<'a> serde::Deserialize<'a>>(&self) -> Result<T> {
        let bytes = self.try_bytes()?;
        Self::deserialize(bytes)
    }

    fn try_parsed_value_modified<T: for<'a> serde::Deserialize<'a>>(
        &self,
        previous_modified: Option<SystemTime>,
    ) -> Result<Option<(T, SystemTime)>> {
        Ok(match self.try_bytes_modified(previous_modified)? {
            Some((bytes, time)) => Some((Self::deserialize(Cow::Owned(bytes))?, time)),
            None => None,
        })
    }

    fn try_load_parsed_value<T: for<'a> serde::Deserialize<'a>>(&self) -> Result<T> {
        let bytes = self.try_load_bytes()?;
        Self::deserialize(Cow::Owned(bytes))
    }

    fn parsed_value<T: for<'a> serde::Deserialize<'a>>(&self) -> T {
        self.try_parsed_value().unwrap()
    }

    fn parsed_value_modified<T: for<'a> serde::Deserialize<'a>>(
        &self,
        previous_modified: Option<SystemTime>,
    ) -> Option<(T, SystemTime)> {
        self.try_parsed_value_modified(previous_modified).unwrap()
    }

    fn load_parsed_value<T: for<'a> serde::Deserialize<'a>>(&self) -> T {
        self.try_load_parsed_value().unwrap()
    }

    fn parsed_value_fallback<T: for<'a> serde::Deserialize<'a>>(&self) -> T {
        self.try_load_parsed_value()
            .unwrap_or_else(|_| self.parsed_value())
    }

    fn try_parsed_value_fallback<T: for<'a> serde::Deserialize<'a>>(&self) -> Result<T> {
        self.try_load_parsed_value()
            .or_else(|_| self.try_parsed_value())
    }
}

pub trait CachedAsset: Asset
where
    Self: 'static + Clone + Eq + Hash,
{
    type CacheType: Cache<Self, Self::Value>;

    fn cache() -> &'static Self::CacheType;

    fn cached(&self) -> Arc<Self::Value> {
        Self::cache().get(self)
    }

    fn cached_modified(&self) -> Arc<Self::Value> {
        Self::cache().get_updated(self)
    }

    fn cached_reload(&self) -> Arc<Self::Value> {
        Self::cache().reload(self)
    }

    fn cached_remove(&self) {
        Self::cache().remove(self);
    }
}
