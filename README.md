# glacé
_(Like the cherries.)_

A proc macro for embedding an entire directory tree into your Rust code in a type-safe, human-friendly, flexible way.

[![CI](https://github.com/mistodon/glace/actions/workflows/rust.yml/badge.svg)](https://github.com/mistodon/glace/actions/workflows/rust.yml)
[![Docs.rs](https://docs.rs/glace/badge.svg)](https://docs.rs/glace/0.3.0/glace/)
[![Crates.io](https://img.shields.io/crates/v/glace.svg)](https://crates.io/crates/glace)
[![codecov](https://codecov.io/gh/mistodon/glace/branch/main/graph/badge.svg?token=5UN70N9TBQ)](https://codecov.io/gh/mistodon/glace)

At its simplest, it provides you with an enum variant matching every file in the tree, so you know at compile-time that the paths to your assets exist. At its most powerful, it can load all of those files into const memory, generate `serde` structs to represent them, provide methods for loading and deserializing (and transparently cacheing) them.

The ideal future of this crate is to fully abstract away the notion of files and allow you to write code as though all of your assets are hard-coded (without, of course, having to hard-code them). We're not there yet, but what's implemented already is useful in its own right.

# Usage

See [docs.rs/glace](docs.rs/glace) (and in particular the _docs_only_example_assets_ module) to get some idea of what `glace` generates for you.

See also the `testcrate` crate in this repo for some examples of how to use the results.

# Some gotchas
1.  The path taken by glace! is relative to the workspace, if a workspace is being used. I don't know how to change that.
