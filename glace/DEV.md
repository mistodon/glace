# Asset management / staticfs

- Refactoring / config improvements
- [ ] Feature to generate docs in the macro, for use in the example
- [ ] Set up testing for multiple feature configurations

- Later
- [ ] `fn asset_from_path(Path) -> dyn Any` that can be downcast
- [ ] Directory layers (mods etc.)
- [ ] Optional `notify` thread for reloading self_caches

Some other ideas:
- For overrides:
    - `ModName<my_module_name>`
    - `Ignore`
    - `NoRecurse`
    - `Image` / `Raw` etc. (to give an Asset type, or ignore one)
    - `PreludeIgnore` don't include it in the prelude module
    - `Mirror<"other/path">` implement Into/From between them
    -  [ ] Transpose
        - [ ] Option mode
        - [ ] Default mode
