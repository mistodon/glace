pub mod cache;
pub mod load;

pub use assist_macros::assist;
pub use indexmap::IndexMap;
pub use lazy_static::lazy_static;

use std::borrow::Cow;
use std::hash::Hash;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;

use parking_lot::RwLock;
use thiserror::Error;

use crate::cache::RwCache;

#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
pub enum PathedKey<'a> {
    Known(&'a str),
    Path { path: &'a Path },
}

#[derive(Debug, Error)]
pub enum AssistError {
    #[error("Path `{0}` is not under the correct parent directory (`{1}`)")]
    MismatchedParent(PathBuf, PathBuf),

    #[error("No Path cached for the given index: `{0}`\nUse the `from_path` method instead of constructing a `_Path(_)` variant directly.")]
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

pub type Result<T> = std::result::Result<T, AssistError>;

pub trait FileAsset: Sized {
    const PARENT: &'static str;

    fn from_const_name(name: &str) -> Option<Self>;
    fn from_const_path(path: &Path) -> Option<Self>;
    fn from_path_unchecked(path: &Path) -> Self;
    fn try_path(self) -> Result<Cow<'static, Path>>;

    fn from_path(path: &Path) -> Self {
        Self::try_from_path(path).unwrap()
    }

    fn try_from_path(path: &Path) -> Result<Self> {
        internal::verify_parent(path, Self::PARENT.as_ref())?;
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

    fn load(bytes: Cow<[u8]>) -> Result<Self::Value>;

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
    fn cache() -> &'static RwCache<Self, Self::Value>;

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

struct PathCache {
    paths: Vec<PathBuf>,
}

impl PathCache {
    fn new() -> Self {
        PathCache {
            paths: Vec::with_capacity(256),
        }
    }

    fn insert_path(&mut self, path: &Path) -> u32 {
        let i = self.paths.len() as u32;
        self.paths.push(path.to_owned());
        i
    }

    fn fetch_path(&self, index: u32) -> Result<&Path> {
        let backing_index = index as usize;
        if backing_index < self.paths.len() {
            Ok(&self.paths[backing_index])
        } else {
            Err(AssistError::NoPathCached(index))
        }
    }

    fn fetch_path_index(&self, path: &Path) -> Option<u32> {
        self.paths.iter().position(|p| p == path).map(|x| x as u32)
    }
}

struct NameCache {
    names: Vec<String>,
}

impl NameCache {
    fn new() -> Self {
        NameCache {
            names: Vec::with_capacity(256),
        }
    }

    fn insert_name(&mut self, name: &str) -> u32 {
        let i = self.names.len() as u32;
        self.names.push(name.to_owned());
        i
    }

    fn fetch_name(&self, index: u32) -> Result<&str> {
        let backing_index = index as usize;
        if backing_index < self.names.len() {
            Ok(&self.names[backing_index])
        } else {
            Err(AssistError::NoNameCached(index))
        }
    }

    fn fetch_name_index(&self, name: &str) -> Option<u32> {
        self.names.iter().position(|p| p == name).map(|x| x as u32)
    }
}

lazy_static::lazy_static! {
    static ref PATH_CACHE: RwLock<PathCache> = RwLock::new(PathCache::new());
    static ref NAME_CACHE: RwLock<NameCache> = RwLock::new(NameCache::new());
}

pub mod internal {
    use super::*;

    use std::time::SystemTime;

    pub fn fetch_path_index(path: &Path) -> u32 {
        let existing = {
            let c = PATH_CACHE.read();
            c.fetch_path_index(path)
        };

        existing.unwrap_or_else(|| {
            let mut c = PATH_CACHE.write();
            c.fetch_path_index(path)
                .unwrap_or_else(|| c.insert_path(path))
        })
    }

    pub fn fetch_name_index(name: &str) -> u32 {
        let existing = {
            let c = NAME_CACHE.read();
            c.fetch_name_index(name)
        };

        existing.unwrap_or_else(|| {
            let mut c = NAME_CACHE.write();
            c.fetch_name_index(name)
                .unwrap_or_else(|| c.insert_name(name))
        })
    }

    pub fn fetch_path(index: u32) -> Result<PathBuf> {
        let c = PATH_CACHE.read();
        c.fetch_path(index).map(Path::to_owned)
    }

    pub fn fetch_name(index: u32) -> Result<String> {
        let c = NAME_CACHE.read();
        c.fetch_name(index).map(str::to_owned)
    }

    pub fn fetch_bytes(index: u32) -> Result<Vec<u8>> {
        let path = fetch_path(index)?;
        Ok(std::fs::read(&path)?)
    }

    pub fn fetch_string(index: u32) -> Result<String> {
        let path = fetch_path(index)?;
        Ok(std::fs::read_to_string(&path)?)
    }

    pub fn visit_files<T, F>(dir: &Path, function: F) -> impl Iterator<Item = T>
    where
        F: Fn(&Path) -> T,
    {
        ignore::WalkBuilder::new(dir)
            .max_depth(Some(1))
            .filter_entry(|entry| {
                let is_file = entry.file_type().map(|f| f.is_file()).unwrap_or(false);
                let skipped = entry
                    .path()
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .map(|s| s.starts_with('_'))
                    .unwrap_or(false);
                is_file && (!skipped || cfg!(feature = "visit_includes_skipped"))
            })
            .build()
            .skip(1)
            .filter_map(|entry| entry.ok())
            .map(move |entry| function(entry.path()))
    }

    pub fn fetch_bytes_modified(
        path: &Path,
        previous_modified: Option<SystemTime>,
    ) -> Result<Option<(Vec<u8>, SystemTime)>> {
        if previous_modified.is_none() || {
            let meta = std::fs::metadata(path)?;
            previous_modified.unwrap() < meta.modified()?
        } {
            use std::io::Read;

            let mut f = std::fs::File::open(path)?;
            let meta = f.metadata()?;
            let modified = meta.modified()?;
            let mut buffer = Vec::with_capacity(meta.len() as usize);
            f.read_to_end(&mut buffer)?;
            return Ok(Some((buffer, modified)));
        }

        Ok(None)
    }

    pub fn verify_parent(child: &Path, parent: &Path) -> Result<()> {
        if !child.starts_with(parent) {
            Err(AssistError::MismatchedParent(
                child.to_owned(),
                parent.to_owned(),
            ))
        } else {
            Ok(())
        }
    }
}

#[cfg(doc)]
assist!(crate, "example_assets", mod example_assets);
