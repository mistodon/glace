mod codegen;
mod compat_tests;
mod config;
mod parsing;

use std::sync::Arc;

use proc_macro::*;
use syn::parse_macro_input;

use crate::{config::Context, parsing::GlaceMacro};

#[proc_macro]
pub fn glace(stream: TokenStream) -> TokenStream {
    use crate::compat_tests::*;
    use std::mem::transmute;

    let crate_dir = std::env::var_os("CARGO_MANIFEST_DIR");
    if let Some(path) = crate_dir {
        std::env::set_current_dir(path).unwrap();
    }

    unsafe {
        size_test::<SizeTest>();
        discriminant_test(transmute::<LayoutTest, u64>(LayoutTest::Zero), 0);
        discriminant_test(transmute::<LayoutTest, u64>(LayoutTest::One), 1);
        discriminant_test(transmute::<LayoutTest, u64>(LayoutTest::Other(123)), 2);
        field_test(transmute::<LayoutTest, u64>(LayoutTest::Other(123)), 123);
    }

    let call: GlaceMacro = parse_macro_input!(stream as GlaceMacro);
    let context = Arc::new(Context::from(call));

    codegen::generate_all_modules(Arc::clone(&context))
        .unwrap()
        .into()
}
