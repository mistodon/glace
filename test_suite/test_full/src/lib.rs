#![allow(dead_code, unused_imports, unused_variables)]

use helper_macros::*;

glace::glace! {
    #[path = "../data"]
    pub mod data {
        "ignored_dir": Ignore,
        "text/ignored_file.txt": Ignore,

        "json/single_json.json": Single,
        "json/virtual_json.json": Virtual,
        "toml/single_toml.toml": Single,
        "toml/virtual_toml.toml": Virtual,
        "yaml/single_yaml.yaml": Single,
        "yaml/virtual_yaml.yaml": Virtual,
    }
}
use data::prelude::*;

mod unused_names {}
use unused_names::*;

enum UnusedVariants {}

// These type definitions prove that glace isn't generating these
// types. If it was, then the names would be ambiguous due to the
// definitions in `unused_names`, and the tests would fail to compile.
// TODO

fn unused_variants() {
    use Text::*;
    use UnusedVariants::*;
}

// These macros implement traits for the given types to prove that
// they weren't already implemented.
// TODO

#[cfg(test)]
mod tests {
    #![warn(dead_code, unused_imports, unused_variables)]

    use super::*;

    #[test]
    fn correct_variants() {
        assert_eq!(Json::ALL, &[Json::FileA, Json::FileB]);
        assert_eq!(VirtualJson::ALL, &[VirtualJson::Key1, VirtualJson::Key2]);
        assert_eq!(SingleJson, SingleJson);

        assert_eq!(Toml::ALL, &[Toml::FileA, Toml::FileB]);
        assert_eq!(VirtualToml::ALL, &[VirtualToml::Key1, VirtualToml::Key2]);
        assert_eq!(SingleToml, SingleToml);

        assert_eq!(Yaml::ALL, &[Yaml::FileA, Yaml::FileB]);
        assert_eq!(VirtualYaml::ALL, &[VirtualYaml::Key1, VirtualYaml::Key2]);
        assert_eq!(SingleYaml, SingleYaml);
    }
}
