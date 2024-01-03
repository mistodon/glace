# BUGS

- [ ] Is Single TOML failing because it's trying to parse as Yaml?

# Asset management / staticfs

- Refactoring / config improvements
- [/] Document crate items
- [/] Feature to generate docs in the macro, for use in the example
- [/] Set up testing for multiple feature configurations

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

## Transpose
I completely forgot the point of this and only just remembered. Say we have a folder structure like:

```
enemies/
  goblin/
    sprite.png
    stats.yaml
  dragon/
    sprite.png
    stats.yaml
```

We would get:

```rust
mod enemies {
    enum Goblin {
        Sprite,
        Stats,
    }
    enum Dragon {
        Sprite,
        Stats,
    }
}
```

Which apart from being weirdly organised does not implement all the proper traits.

Instead, with Transpose, we would get:

```rust
mod enemies {
    enum Enemies {
        Goblin,
        Dragon,
    }
    enum Sprite {
        Goblin,
        Dragon,
    }
    enum Stats {
        Goblin,
        Dragon,
    }
}
```

Maybe with an extra mapping from Enemies -> Sprite/Stats would be helpful.
