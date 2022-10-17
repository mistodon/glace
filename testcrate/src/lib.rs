#[derive(serde::Deserialize)]
pub struct ItemData {
    pub name: String,
    pub rarity: usize,
}

#[derive(serde::Deserialize)]
pub struct Config {
    pub name: String,
    pub opt: bool,
}

assist::assist! {"assets", mod assets
where
    "assets/configs": Serde<Config>,
    "assets/items.yaml": Virtual + Serde<ItemData>,
    "assets/languages": Transpose,
}

#[cfg(test)]
mod pathtests {
    use super::*;
    use assist::FileAsset;

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

    use assist::BytesAsset;
    use assist::FileAsset;

    #[allow(unused_imports)]
    use super::assets::{
        self,
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
        let json = [Json::FirstJsonFile, Json::SecondJsonFile];

        assert_eq!(&sprites, Sprites::ALL);
        assert_eq!(&notes, Notes::ALL);
        assert_eq!(&json, Json::ALL);

        // TODO: Once virtual is implemented, this should be fixed
        //assert_eq!(assets::Assets::ALL, &[]);
        assert_eq!(assets::text::Text::ALL, &[]);
    }

    #[test]
    fn file_paths_are_correct() {
        assert_eq!(Sprites::Happy.const_path(), "assets/sprites/happy.png");
        assert_eq!(Sprites::Sad.const_path(), "assets/sprites/sad.png");
        assert_eq!(Notes::Note1.const_path(), "assets/text/notes/note1.txt");
        assert_eq!(Notes::Note2.const_path(), "assets/text/notes/note2.txt");
        assert_eq!(
            Json::FirstJsonFile.const_path(),
            "assets/text/json/first_json_file.json"
        );
        assert_eq!(
            Json::SecondJsonFile.const_path(),
            "assets/text/json/Second JSON file.json"
        );
    }

    #[test]
    fn file_bytes_are_correct() {
        assert_eq!(Notes::Note1.const_bytes(), b"Note 1\n");
        assert_eq!(Notes::Note2.const_bytes(), b"Note 2\n");
        assert_eq!(
            Json::FirstJsonFile.const_bytes(),
            b"{\n    \"name\": \"one\"\n}\n"
        );
    }

    #[test]
    fn from_path_functions_correctly() {
        assert_eq!(
            Notes::from_const_path("assets/text/notes/note1.txt".as_ref()),
            Some(Notes::Note1)
        );
        assert_eq!(
            Notes::from_path("assets/text/notes/note1.txt".as_ref()),
            Notes::Note1
        );
        assert_eq!(
            Notes::from_path("assets/text/notes/_secret_note.txt".as_ref()),
            Notes::_Path(0)
        );
        assert_eq!(
            Notes::from_path("assets/text/notes/_another_secret_note.txt".as_ref()),
            Notes::_Path(1)
        );
    }

    #[test]
    fn path_works_correctly() {
        let secret_path: &Path = "assets/text/notes/_secret_note.txt".as_ref();
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
        let secret_path: &Path = "assets/text/notes/_secret_note.txt".as_ref();
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
            Cow::Owned("assets/text/notes/_secret_note.txt".into()),
            Cow::Owned("assets/text/notes/_another_secret_note.txt".into()),
        ];

        let mut actual = Notes::all_paths().collect::<Vec<_>>();

        expected.sort();
        actual.sort();

        assert_eq!(actual, expected);
    }

    #[test]
    fn load_modified_works() {
        let secret_path: &Path = "assets/text/notes/_secret_note.txt".as_ref();
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
        use assist::Asset;

        let _img: assist::load::RgbaImage = Sprites::Happy.value();
    }

    #[test]
    fn json_impl() {
        use assist::SerdeAsset;

        #[derive(serde::Deserialize)]
        struct Thing {
            name: String,
        }

        let t = Json::FirstJsonFile.parsed_value::<Thing>();
        assert_eq!(t.name, "one");
    }

    #[test]
    fn specific_serde_impl() {
        use assist::Asset;

        let dev: Config = assets::configs::Configs::Dev.value();
        let release: Config = assets::configs::Configs::Release.value();

        assert_eq!(dev.name, "dev");
        assert_eq!(release.name, "release");
        assert!(!dev.opt);
        assert!(release.opt);
    }
}
