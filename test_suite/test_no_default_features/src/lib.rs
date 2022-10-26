#![allow(dead_code, unused_imports, unused_variables)]

use helper_macros::*;

glace::glace! {
    #[path = "../data"]
    pub mod data {
        "ignored_dir": Ignore,
        "text/ignored_file.txt": Ignore,
    }
}
use data::prelude::*;

mod unused_names {
    pub type IgnoredDir = ();
}
use unused_names::*;

enum UnusedVariants {
    IgnoredFile,
}

// These type definitions prove that glace isn't generating these
// types. If it was, then the names would be ambiguous due to the
// definitions in `unused_names`, and the tests would fail to compile.
type T0 = IgnoredDir;

fn unused_variants() {
    use Text::*;
    use UnusedVariants::*;

    let v = IgnoredFile;
}

// These macros implement traits for the given types to prove that
// they weren't already implemented.
not_bytes_asset!(Images, Json, Text, NestedText, Toml, Yaml);

#[cfg(test)]
mod tests {
    #![warn(dead_code, unused_imports, unused_variables)]

    use super::*;

    use std::path::Path;

    #[test]
    fn const_path() {
        assert_eq!(Images::Happy.const_path(), "../data/images/happy.png");
        assert_eq!(Text::FileA.const_path(), "../data/text/file_a.txt");
        assert_eq!(
            NestedText::NestedA.const_path(),
            "../data/text/nested_text/nested_a.txt"
        );
    }

    #[test]
    fn const_name() {
        assert_eq!(Images::Happy.const_name(), "Happy");
        assert_eq!(Text::FileA.const_name(), "FileA");
        assert_eq!(NestedText::NestedA.const_name(), "NestedA");
    }

    #[test]
    fn all_const() {
        assert_eq!(Images::ALL, &[Images::Happy, Images::Sad]);
        assert_eq!(NestedText::ALL, &[NestedText::NestedA, NestedText::NestedB]);
    }

    #[test]
    fn from_const_name() {
        assert_eq!(Images::from_const_name("Happy"), Some(Images::Happy));
        assert_eq!(Images::from_const_name("Confused"), None);
    }

    #[test]
    fn from_const_path() {
        assert_eq!(
            Images::from_const_path("../data/images/happy.png"),
            Some(Images::Happy)
        );
        assert_eq!(Images::from_const_path("../data/images/confused.png"), None);
    }

    #[test]
    fn file_path() {
        let p = Images::Happy.path();
        let s: &Path = "../data/images/happy.png".as_ref();
        assert_eq!(p.as_ref(), s);

        let p = Images::from_path_unchecked("../data/fake_path").path();
        let s: &Path = "../data/fake_path".as_ref();
        assert_eq!(p.as_ref(), s);
    }

    #[test]
    fn file_from_path() {
        let known = Images::try_from_path("../data/images/happy.png");
        let unknown = Images::try_from_path("../data/images/fake.png");
        let invalid = Images::try_from_path("wrong_parent/file.png");

        assert_eq!(known.unwrap(), Images::Happy);
        assert!(matches!(unknown, Ok(Images::_Path(_))));
        assert!(invalid.is_err());
    }
}
