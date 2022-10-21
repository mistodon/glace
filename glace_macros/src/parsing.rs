use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use color_eyre::eyre::{eyre, Result};
use convert_case::{Case, Casing};
use indexmap::IndexMap;
use proc_macro::*;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse::{Error, Parse, ParseStream, Result as ParseResult},
    parse_macro_input,
    punctuated::Punctuated,
    Ident, Token, Type,
};

use crate::codegen::ident;

pub struct GlaceMacro {
    pub alias: Ident,
    pub path: PathBuf,
    pub mod_name: String,
    pub overrides: HashMap<PathBuf, Vec<Property>>,
}

impl Parse for GlaceMacro {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let mut alias = ident("glace");
        if input.parse::<Token![crate]>().is_ok() {
            alias = ident("crate");
            input.parse::<Token![,]>()?;
        } else if let Ok(token) = input.parse::<syn::Ident>() {
            alias = token;
            input.parse::<Token![,]>()?;
        }
        let path = input.parse::<syn::LitStr>()?.value().into();
        input.parse::<Token![,]>()?;
        input.parse::<Token![mod]>()?;
        let mod_name: Ident = input.parse()?;
        let mod_name = mod_name.to_string();

        let mut overrides = HashMap::new();
        if input.parse::<Token![where]>().is_ok() {
            let overs: Punctuated<Override, Token![,]> = Punctuated::parse_terminated(input)?;
            for o in overs.into_iter() {
                overrides.insert(o.path, o.properties.into_iter().collect());
            }
        }

        Ok(GlaceMacro {
            alias,
            path,
            mod_name,
            overrides,
        })
    }
}

pub struct Override {
    pub path: PathBuf,
    pub properties: Punctuated<Property, Token![+]>,
}

impl Parse for Override {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let path = input.parse::<syn::LitStr>()?.value().into();
        input.parse::<Token![:]>()?;
        let properties = Punctuated::parse_separated_nonempty(input)?;
        Ok(Override { path, properties })
    }
}

pub enum Property {
    Single,
    Virtual,
    WithCache,
    NoCache,
    WithConst,
    NoConst,
    WithIO,
    NoIO,
    // WithSerde,
    // NoSerde,
    // WithEdres,
    // NoEdres,
    // Transpose(TransposeType),
    #[cfg(feature = "serde")]
    Serde(Box<Type>),
}

// pub enum TransposeType {
//     Required,
//     Option,
//     Default,
// }

impl Parse for Property {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let ident = input.parse::<syn::Ident>()?;

        let value = match ident.to_string().as_str() {
            "Single" => Property::Single,
            "Virtual" => Property::Virtual,
            "WithCache" => Property::WithCache,
            "NoCache" => Property::NoCache,
            "WithConst" => Property::WithConst,
            "NoConst" => Property::NoConst,
            "WithIO" => Property::WithIO,
            "NoIO" => Property::NoIO,
            // "WithSerde" => Property::WithSerde,
            // "NoSerde" => Property::NoSerde,
            // "WithEdres" => Property::WithEdres,
            // "NoEdres" => Property::NoEdres,
            // "Transpose" => {
            //     let mut kind = TransposeType::Required;
            //     if input.parse::<Token![<]>().is_ok() {
            //         let ident = input.parse::<syn::Ident>()?;
            //         kind = match ident.to_string().as_str() {
            //             "Required" => TransposeType::Required,
            //             "Default" => TransposeType::Default,
            //             "Option" => TransposeType::Option,
            //             _ => return Err(Error::new(ident.span(), "Unrecognized transpose type")),
            //         };
            //         input.parse::<Token![>]>()?;
            //     }
            //     Property::Transpose(kind)
            // }
            #[cfg(feature = "serde")]
            "Serde" => {
                input.parse::<Token![<]>()?;
                let r#type = input.parse::<Type>()?;
                input.parse::<Token![>]>()?;
                Property::Serde(Box::new(r#type))
            }
            _ => return Err(Error::new(ident.span(), "Unrecognized override for path")),
        };

        Ok(value)
    }
}
