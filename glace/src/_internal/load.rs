use indexmap::IndexMap;
use std::borrow::Cow;
use std::hash::Hash;

use crate::Result;

#[cfg(feature = "image")]
pub use image::RgbaImage;

//#[cfg(feature = "serde")]
use serde::Deserialize;

#[cfg(feature = "image")]
pub fn load_image(bytes: Cow<[u8]>) -> Result<RgbaImage> {
    Ok(image::load_from_memory(&bytes).expect("TODO").to_rgba8())
}

#[cfg(feature = "serde_json")]
pub fn load_json<T: for<'a> Deserialize<'a>>(bytes: Cow<[u8]>) -> Result<T> {
    Ok(serde_json::from_slice(bytes.as_ref()).expect("TODO"))
}

#[cfg(feature = "serde_toml")]
pub fn load_toml<T: for<'a> Deserialize<'a>>(bytes: Cow<[u8]>) -> Result<T> {
    Ok(toml::from_slice(bytes.as_ref()).expect("TODO"))
}

#[cfg(feature = "serde_yaml")]
pub fn load_yaml<T: for<'a> Deserialize<'a>>(bytes: Cow<[u8]>) -> Result<T> {
    Ok(serde_yaml::from_slice(bytes.as_ref()).expect("TODO"))
}

#[cfg(feature = "serde_json")]
pub fn load_all_json<T: Eq + Hash + for<'a> Deserialize<'a>>(
    source: &[u8],
) -> Result<IndexMap<T, Vec<u8>>> {
    let map: IndexMap<T, &serde_json::value::RawValue> =
        serde_json::from_slice(source).expect("TODO");
    Ok(map
        .into_iter()
        .map(|(k, v)| (k, v.get().as_bytes().to_owned()))
        .collect())
}

#[cfg(feature = "serde_toml")]
pub fn load_all_toml<T: Eq + Hash + for<'a> Deserialize<'a>>(
    source: &[u8],
) -> Result<IndexMap<T, Vec<u8>>> {
    let map: IndexMap<T, toml::Value> = toml::from_slice(source).expect("TODO");
    Ok(map
        .into_iter()
        .map(|(k, v)| (k, toml::to_vec(&v).expect("TODO")))
        .collect())
}

#[cfg(feature = "serde_yaml")]
pub fn load_all_yaml<T: Eq + Hash + for<'a> Deserialize<'a>>(
    source: &[u8],
) -> Result<IndexMap<T, Vec<u8>>> {
    let map: IndexMap<T, serde_yaml::Value> = serde_yaml::from_slice(source).expect("TODO");
    Ok(map
        .into_iter()
        .map(|(k, v)| (k, serde_yaml::to_string(&v).expect("TODO").into()))
        .collect())
}
