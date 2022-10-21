# Asset management / staticfs

- [ ] Document the public API
- [x] Comment out incomplete features
- [x] Make serde (temporarily) non-optional
- [x] Make a proper README
- [ ] And a proper Cargo.toml
- [ ] Release 0.1.0

- [ ] Set up testing for multiple feature configurations
- [ ] Per-module config (taking into account features and overrides)

- [ ] Per-path overrides:
    - [ ] Transpose
        - [ ] Option mode
        - [ ] Default mode
        - `Serde<MyType>`
    - [ ] disable_const / disable_fs / disable_cache overrides
        - `WithConst`, `WithIo`, `NoConst`, `NoIo`, `WithCache`, `NoCache`
        - see below...
- [ ] Maybe also export the glace:: traits in prelude?
- [ ] Path-to `dyn BytesAsset` that can be downcast
- [ ] Directory layers (mods etc.)
- [ ] Optional `notify` thread for reloading self_caches

New syntax:
```rust
glace!{
    #[path = "path/to/assets"]
    pub mod my_asset_module_name { // Any visibility specifier ok
        use whatever::path as glace; // Optional

        "my_file_to_override.txt": Single,
        "my_dir_to_override": Serde<MyType>,
    }
}
```

Some other ideas:
- For overrides:
    - `ModName<my_module_name>`
    - `Ignore`
    - `NoRecurse`
    - `Image` / `Raw` etc. (to give an Asset type, or ignore one)
    - `PreludeIgnore` don't include it in the prelude module
- Implement Asset<Cow<[u8]>> or Asset<Cow<str>> for assets without another implementation.
