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

    #[test]
    fn serialization() {
        let file_variant = Yaml::FileA;
        let missing_file = Yaml::from_path("../data/yaml/fake.yaml");
        let virtual_variant = VirtualYaml::Key1;
        let missing_virtual = VirtualYaml::from_name("Fake");
        let single_yaml = SingleYaml;

        assert_eq!(serde_yaml::to_string(&[file_variant]).unwrap(), "---\n- FileA\n");
        assert_eq!(serde_yaml::to_string(&[missing_file]).unwrap(), "---\n- path: \"../data/yaml/fake.yaml\"\n");
        assert_eq!(serde_yaml::to_string(&[virtual_variant]).unwrap(), "---\n- Key1\n");
        assert_eq!(serde_yaml::to_string(&[missing_virtual]).unwrap(), "---\n- Fake\n");
        assert_eq!(serde_yaml::to_string(&[single_yaml]).unwrap(), "---\n- ~\n");
    }

    #[test]
    fn deserialization() {
        let file_variant = "- FileA";
        let missing_file = "- path: \"../data/yaml/fake.yaml\"";
        let virtual_variant = "- Key1";
        let missing_virtual = "- Fake";
        let single_yaml = "- ~";

        let file_variant: Vec<Yaml> = serde_yaml::from_str(file_variant).unwrap();
        let missing_file: Vec<Yaml> = serde_yaml::from_str(missing_file).unwrap();
        let virtual_variant: Vec<VirtualYaml> = serde_yaml::from_str(virtual_variant).unwrap();
        let missing_virtual: Vec<VirtualYaml> = serde_yaml::from_str(missing_virtual).unwrap();
        let single_yaml: Vec<SingleYaml> =  serde_yaml::from_str(single_yaml).unwrap();

        assert_eq!(file_variant, vec![Yaml::FileA]);
        assert_eq!(missing_file, vec![Yaml::_Unknown(0)]);
        assert_eq!(virtual_variant, vec![VirtualYaml::Key1]);
        assert_eq!(missing_virtual, vec![VirtualYaml::_Unknown(0)]);
        assert_eq!(single_yaml, vec![SingleYaml]);
    }
}
