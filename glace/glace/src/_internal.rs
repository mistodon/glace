pub mod load;

use std::{
    path::{Path, PathBuf},
    time::SystemTime,
};

use crate::{GlaceError, Result};
use parking_lot::RwLock;

lazy_static::lazy_static! {
    static ref PATH_CACHE: RwLock<PathCache> = RwLock::new(PathCache::new());
    static ref NAME_CACHE: RwLock<NameCache> = RwLock::new(NameCache::new());
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
            Err(GlaceError::NoPathCached(index))
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
            Err(GlaceError::NoNameCached(index))
        }
    }

    fn fetch_name_index(&self, name: &str) -> Option<u32> {
        self.names.iter().position(|p| p == name).map(|x| x as u32)
    }
}

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
        .sort_by_file_name(std::ffi::OsStr::cmp)
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
        Err(GlaceError::MismatchedParent(
            child.to_owned(),
            parent.to_owned(),
        ))
    } else {
        Ok(())
    }
}
