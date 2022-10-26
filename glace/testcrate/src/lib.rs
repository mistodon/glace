pub use glace; // Only exported to test aliasing

use glace::serde;

#[derive(Debug, PartialEq, Eq, serde::Deserialize)]
pub struct ItemData {
    pub name: String,
    pub rarity: usize,
}

#[derive(serde::Deserialize)]
pub struct Profile {
    pub name: String,
    pub opt: bool,
}

#[derive(Debug, PartialEq, Eq, serde::Deserialize)]
pub struct Config {
    pub name: String,
    pub description: String,
}

glace::glace! {
    #[path = "assets"]
    pub mod assets {
        use crate::glace as glace; // Only included to test aliasing

        "autoconfig.toml": Single,
        "autoitems.yaml": Virtual,

        "config.toml": Single + Serde<Config>,
        "items.yaml": Virtual + Serde<ItemData>,
        "profiles": Serde<Profile>,

        // "languages": Transpose,
    }
}

#[cfg(test)]
mod pathtests {
    use super::*;
    use glace::FileAsset;

    #[test]
    fn modules_have_correct_paths() {
        assert_eq!(assets::Assets::PARENT, "assets");
        assert_eq!(assets::sprites::Sprites::PARENT, "assets/sprites");
        assert_eq!(assets::text::notes::Notes::PARENT, "assets/text/notes");
        assert_eq!(assets::text::json::Json::PARENT, "assets/text/json");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::borrow::Cow;
    use std::path::Path;
    use std::time::SystemTime;

    use glace::BytesAsset;
    use glace::FileAsset;

    #[allow(unused_imports)]
    use super::assets::{
        self,
        items::Items,
        sprites::{self, Sprites},
        text::{
            self,
            json::{self, Json},
            notes::{self, Notes},
        },
    };

    #[test]
    fn file_variants_are_generated_correctly() {
        let sprites = [Sprites::Happy, Sprites::Sad];
        let notes = [Notes::Note1, Notes::Note2];
        let json = [Json::PrimaryJsonFile, Json::SecondaryJsonFile];

        assert_eq!(&sprites, Sprites::ALL);
        assert_eq!(&notes, Notes::ALL);
        assert_eq!(&json, Json::ALL);

        assert_eq!(assets::Assets::ALL, &[]);
        assert_eq!(assets::text::Text::ALL, &[]);
    }

    #[test]
    fn file_paths_are_correct() {
        assert_eq!(Sprites::Happy.const_path(), "assets/sprites/happy.png");
        assert_eq!(Sprites::Sad.const_path(), "assets/sprites/sad.png");
        assert_eq!(Notes::Note1.const_path(), "assets/text/notes/note1.txt");
        assert_eq!(Notes::Note2.const_path(), "assets/text/notes/note2.txt");
        assert_eq!(
            Json::PrimaryJsonFile.const_path(),
            "assets/text/json/primary_json_file.json"
        );
        assert_eq!(
            Json::SecondaryJsonFile.const_path(),
            "assets/text/json/secondary JSON File.json"
        );
    }

    #[test]
    fn file_bytes_are_correct() {
        assert_eq!(Notes::Note1.const_bytes(), b"Note 1\n");
        assert_eq!(Notes::Note2.const_bytes(), b"Note 2\n");
        assert_eq!(
            Json::PrimaryJsonFile.const_bytes(),
            b"{\n    \"name\": \"one\"\n}\n"
        );
    }

    #[test]
    fn from_path_functions_correctly() {
        assert_eq!(
            Notes::from_const_path("assets/text/notes/note1.txt"),
            Some(Notes::Note1)
        );
        assert_eq!(
            Notes::from_path("assets/text/notes/note1.txt"),
            Notes::Note1
        );
        assert_eq!(
            Notes::from_path("assets/text/notes/_a_secret_note.txt"),
            Notes::_Path(0)
        );
        assert_eq!(
            Notes::from_path("assets/text/notes/_another_secret_note.txt"),
            Notes::_Path(1)
        );
    }

    #[test]
    fn path_works_correctly() {
        let secret_path: &Path = "assets/text/notes/_a_secret_note.txt".as_ref();
        let another_secret_path: &Path = "assets/text/notes/_another_secret_note.txt".as_ref();

        let secret_note = Notes::from_path(secret_path);
        let another_secret_note = Notes::from_path(another_secret_path);

        assert_eq!(
            Notes::Note1.path(),
            Cow::<'static, Path>::Borrowed("assets/text/notes/note1.txt".as_ref())
        );
        assert_eq!(
            Notes::Note2.path(),
            Cow::<'static, Path>::Borrowed("assets/text/notes/note2.txt".as_ref())
        );
        assert_eq!(
            secret_note.path(),
            Cow::<'static, Path>::Owned(secret_path.to_owned())
        );
        assert_eq!(
            another_secret_note.path(),
            Cow::<'static, Path>::Owned(another_secret_path.to_owned())
        );
    }

    #[test]
    fn bytes_works_correctly() {
        let secret_path: &Path = "assets/text/notes/_a_secret_note.txt".as_ref();
        let secret_note = Notes::from_path(secret_path);

        assert_eq!(Notes::Note1.bytes(), Cow::Borrowed(b"Note 1\n"));
        assert_eq!(
            secret_note.bytes(),
            Cow::<'static, [u8]>::Owned(b"This is a secret.\n".to_vec())
        );
    }

    #[test]
    fn all_variants_works() {
        let expected = vec![Notes::Note1, Notes::Note2, Notes::_Path(0), Notes::_Path(1)];
        let mut actual = Notes::all_variants().collect::<Vec<_>>();
        actual.sort();

        assert_eq!(actual, expected);
    }

    #[test]
    fn all_paths_works() {
        let mut expected: Vec<Cow<'static, Path>> = vec![
            Cow::Borrowed("assets/text/notes/note1.txt".as_ref()),
            Cow::Borrowed("assets/text/notes/note2.txt".as_ref()),
            Cow::Owned("assets/text/notes/_a_secret_note.txt".into()),
            Cow::Owned("assets/text/notes/_another_secret_note.txt".into()),
        ];

        let mut actual = Notes::all_paths().collect::<Vec<_>>();

        expected.sort();
        actual.sort();

        assert_eq!(actual, expected);
    }

    #[test]
    fn load_modified_works() {
        let secret_path: &Path = "assets/text/notes/_a_secret_note.txt".as_ref();
        let secret_note = Notes::from_path(secret_path);

        let (_, time1) = Notes::Note1.bytes_modified(None).unwrap();
        let (_, time2) = secret_note.bytes_modified(None).unwrap();

        assert!(Notes::Note1.bytes_modified(Some(time1)).is_none());
        assert!(secret_note.bytes_modified(Some(time2)).is_none());
        assert!(Notes::Note1
            .bytes_modified(Some(SystemTime::UNIX_EPOCH))
            .is_some());
    }

    #[test]
    fn image_impl() {
        use glace::Asset;

        let _img: glace::image::RgbaImage = Sprites::Happy.value();
    }

    #[test]
    fn json_impl() {
        use glace::SerdeAsset;

        #[derive(serde::Deserialize)]
        struct Thing {
            name: String,
        }

        let t = Json::PrimaryJsonFile.parsed_value::<Thing>();
        assert_eq!(t.name, "one");
    }

    #[test]
    fn specific_serde_impl() {
        use glace::Asset;

        let dev: Profile = assets::profiles::Profiles::Dev.value();
        let release: Profile = assets::profiles::Profiles::Release.value();

        assert_eq!(dev.name, "dev");
        assert_eq!(release.name, "release");
        assert!(!dev.opt);
        assert!(release.opt);
    }

    #[test]
    fn virtual_impl() {
        use glace::Asset;
        use glace::CachedAsset;

        let item_1 = assets::items::Items::Item1.value();
        let item_2 = assets::items::Items::Item2.value();
        let item_1c = assets::items::Items::Item1.cached();
        let item_2c = assets::items::Items::Item2.cached();
        assert_eq!(item_1.name, "Item one");
        assert_eq!(item_2.name, "Item two");
        assert_eq!(item_1.rarity, 1);
        assert_eq!(item_2.rarity, 2);
        assert_eq!(item_1, *item_1c);
        assert_eq!(item_2, *item_2c);
    }

    #[test]
    fn edres_impl() {
        use glace::Asset;
        use glace::CachedAsset;

        let item = assets::autoprofiles::Autoprofiles::Dev;
        let value: assets::autoprofiles::AutoprofilesValue = item.value();
        let cached_value = item.cached();

        assert_eq!(value.name, "dev");
        assert_eq!(value.opt, false);
        assert_eq!(value, *cached_value);
    }

    #[test]
    fn edres_impl_single() {
        use glace::Asset;
        use glace::CachedAsset;

        let config = assets::autoconfig::Autoconfig;
        let value: assets::autoconfig::AutoconfigValue = config.value();
        let cached_value = config.cached();

        assert_eq!(value.name, "Config");
        assert_eq!(value.description, "A config file.");
        assert_eq!(value, *cached_value);
    }

    #[test]
    fn edres_impl_virtual() {
        use glace::Asset;
        use glace::CachedAsset;

        let item = assets::autoitems::Autoitems::Item1;
        let value: assets::autoitems::AutoitemsValue = item.value();
        let cached_value = item.cached();

        assert_eq!(value.name, "Item one");
        assert_eq!(value.rarity, 1);
        assert_eq!(value, *cached_value);
    }

    #[test]
    fn prelude() {
        use assets::prelude::*;
        use glace::Asset;

        assert_eq!(Items::Item1.value().name, "Item one");
        assert_eq!(Json::SecondaryJsonFile.value().name, "two");
    }
}
