#[cfg(feature = "image")]
use image::RgbaImage;

#[cfg(feature = "serde")]
use serde::Deserialize;

#[cfg(feature = "image")]
pub fn load_image(bytes: std::borrow::Cow<[u8]>) -> crate::Result<RgbaImage> {
    Ok(image::load_from_memory(&bytes).expect("TODO").to_rgba8())
}

#[cfg(feature = "serde_json")]
pub fn load_json<T: for<'a> Deserialize<'a>>(bytes: std::borrow::Cow<[u8]>) -> crate::Result<T> {
    Ok(serde_json::from_slice(bytes.as_ref()).expect("TODO"))
}

#[cfg(feature = "serde_toml")]
pub fn load_toml<T: for<'a> Deserialize<'a>>(bytes: std::borrow::Cow<[u8]>) -> crate::Result<T> {
    Ok(toml::from_slice(bytes.as_ref()).expect("TODO"))
}

#[cfg(feature = "serde_yaml")]
pub fn load_yaml<T: for<'a> Deserialize<'a>>(bytes: std::borrow::Cow<[u8]>) -> crate::Result<T> {
    Ok(serde_yaml::from_slice(bytes.as_ref()).expect("TODO"))
}

#[cfg(feature = "serde_json")]
pub fn load_all_json<T: Eq + std::hash::Hash + for<'a> Deserialize<'a>>(
    source: &[u8],
) -> crate::Result<indexmap::IndexMap<T, Vec<u8>>> {
    let map: indexmap::IndexMap<T, &serde_json::value::RawValue> =
        serde_json::from_slice(source).expect("TODO");
    Ok(map
        .into_iter()
        .map(|(k, v)| (k, v.get().as_bytes().to_owned()))
        .collect())
}

#[cfg(feature = "serde_toml")]
pub fn load_all_toml<T: Eq + std::hash::Hash + for<'a> Deserialize<'a>>(
    source: &[u8],
) -> crate::Result<indexmap::IndexMap<T, Vec<u8>>> {
    let map: indexmap::IndexMap<T, toml::Value> = toml::from_slice(source).expect("TODO");
    Ok(map
        .into_iter()
        .map(|(k, v)| (k, toml::to_vec(&v).expect("TODO")))
        .collect())
}

#[cfg(feature = "serde_yaml")]
pub fn load_all_yaml<T: Eq + std::hash::Hash + for<'a> Deserialize<'a>>(
    source: &[u8],
) -> crate::Result<indexmap::IndexMap<T, Vec<u8>>> {
    let map: indexmap::IndexMap<T, serde_yaml::Value> =
        serde_yaml::from_slice(source).expect("TODO");
    Ok(map
        .into_iter()
        .map(|(k, v)| (k, serde_yaml::to_string(&v).expect("TODO").into()))
        .collect())
}
