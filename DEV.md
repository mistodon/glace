# Asset management / staticfs

Actual todos:
1. [x] Implement ImageAsset only for images
2. [x] Implement JsonAsset only for json files
3. [x] Finish implementing RwCache and add self_cached feature
    - [x] Make sure cache can fall through to const data
    - [x] And do read-if-modified
4. [x] Add disable_fs and disable_const_data features
    - disable_const_data should remove const fns and change the behavious of fallbacky methods to always load
    - disable_fs should always and only fall back to const (or directly return Err?)
    - disable both just un-implements all traits
5. [x] Ensure load_modified works with fs disabled (fallback)
6. [x] Ensure cache in general works with fs disabled
7. [x] Make self_cached fns into a CachedAsset trait

Nice-to-haves:
- [x] Faster RwLocks
- [x] Other serde crate supports
- [ ] Per-path overrides:
    - [x] Single (generates just struct instead of enum)
    - [x] Virtual
    - [ ] Transpose
        - [ ] Option mode
        - [ ] Default mode
    - [x] Explicit supplied deserialized type
        - `Serde<MyType>`
    - [ ] disable_const / disable_fs / disable_cache overrides
        - `WithConst`, `WithIo`, `NoConst`, `NoIo`, `WithCache`, `NoCache`
- [ ] Generate prelude module that exports all data types
    - And maybe also export the assist:: traits
- [x] Optional edres type generation
- [ ] Path-to `dyn BytesAsset` that can be downcast
- [ ] Directory layers (mods etc.)
- [ ] Optional `notify` thread for reloading self_caches

Notes:
- Override syntax:
    ```rust
    assist!("path", mod assets, where
        "path/items.yaml": Unpack,
        "path/monsters": Json<MonsterData>,
        "path/skills.yaml": Unpack + Json<MonsterData>,
        "path/entities": Transpose,
        "path/dynamic": DisableCache,
        "path/immutable": DisableFs,
        "path/large_assets": DisableConst,
    );
    ```

Requirements, in order of how sure I am:

1. [x] Auto-generate identifiers for all assets at compile time
2. [x] Allow for construction of identifiers for assets that weren't there at compile time (an 'other' variant in the enum or something)
3. [x] Identifier -> Path function
4. [x] Path -> Identifier
5. [x] Enum -> list all compile-time variants
6. [x] Enum -> list all variants, including new ones on disk
7. [x] Identifier -> const str/bytes
8. [x] Identifier -> load str/bytes from disk
9. [x] Identifier -> load either const/disk (as a Cow)
10. [x] Identifier -> load only if changed
11. [ ] Only generate loaders for sensible extensions (only implement JsonAsset for .json files)
11. [/] Identifier -> load parsed value (edres struct, image, etc.)
    - from const / disk / Cow
12. [/] Optional loader extensions (if they're images, generate a set of image, try_image, load_image etc.)
    - see image::ImageFormat::from_path
13. [/] Optional deser extensions (if they're json, generate a set of json<T>, try_json<T>, etc.)
    - assume based on extension
14. [ ] Optional "virtual directories" override per-file:
    - So treat the specified file as though it's a directory, and it's "file names" are the keys of the config file it is, and the "file contents" are the values associated with those keys. So overriding items.yaml would give you an Items enum, where str/bytes/value would refer to the contents within that file.
15. [ ] Optional edres extension (if they're json, generate a value type with edres and have value, try_value, etc.)
16. [/] traits and Cache types that you can use
17. [ ] auto-generate Caches for each asset
20. [ ] Multiple configurable "layers"
    - e.g. compile-time only
    - or local dir with fallback to compile-time
    - or mods dir with fallback to game assets dir with additional fallback to compile time data
    - This could be implemented with single path_chain() -> Iterator + try_load_bytes_chain() + try_bytes_fallback_chain()
    - The hard part is configuring the search paths
25. [ ] Transpose - each directory is a variant, all files with a certain name are the enum
30. [ ] Untyped Identifier?? (In case you want to mix and match sprites and audio or whatever.)

Dude though, what if it's a trait?

so

```rust
struct Cache<K: Asset<V>, V> {
    fn: gimme it, load it if you have to
    fn: gimme it if its changed since a certain time
    fn: gimme it, load it unconditionally (invalidation then fetch)
    fn: remove it from cache (invalidation)
}

impl Asset<Cow<[u8]>> for BytesAsset;
impl Asset<Cow<str>> for StrAsset;

With self_cache feature, add:
fn cached
fn cached_modified
fn cached_reload
fn uncache

to the asset itself, which all access a lazy_static Cache
```

```rust
{
    let lock = cache.read();
    if let lock.get(key):
        return
}
let data = key.load(); // May lock pathcache, but its ok.
{
    let lock = cache.write();
    lock.put(key, data);
}
Some(data)
```

Cache thread_local instead of using locks??

# Transpose design

language/
    en/
        description.txt
        credits.txt
    it/
        description.txt
        credits.txt

If we say that `"assets/language": Transpose`, then the simplest implementation would be:

```rust
mod language {
    mod description {
        pub enum Description {
            En,
            It,
        }
    }

    mod credits {
        pub enum Credits {
            En,
            It,
        }
    }
}
```

But this deprives us of an umbrella key that we can use to access both description and credits. So consider also adding:

```rust
    pub enum Language {
        En,
        It,
    }

    impl Language {
        pub fn description(self) -> Description {
            ...
        }

        pub fn credits(self) -> Credits {
            ...
        }
    }
```

This would necessitate that Transpose directories _only_ contain other directories.

This also raises the question of what happens if a subdirectory is missing a file.
