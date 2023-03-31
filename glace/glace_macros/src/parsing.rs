use std::collections::HashMap;
use std::path::PathBuf;

use syn::{
    parse::{Error, Parse, ParseStream, Result as ParseResult},
    punctuated::Punctuated,
    Attribute, Ident, Token, Visibility,
};

use crate::codegen::ident;

fn validate_path_attribute(attributes: &[Attribute]) -> ParseResult<PathBuf> {
    use syn::{Lit, Meta};

    if attributes.len() == 1 {
        let meta = attributes[0].parse_meta()?;
        if let Meta::NameValue(nv) = meta {
            if nv.path.is_ident(&ident("path")) {
                if let Lit::Str(path) = nv.lit {
                    return Ok(path.value().into());
                }
            }
        }
    }

    panic!(r#"Expected exactly one `#[path = "..."]` attribute"#)
}

pub struct GlaceMacro {
    pub alias: Option<syn::Path>,
    pub visibility: Option<Visibility>,
    pub path: PathBuf,
    pub mod_name: String,
    pub overrides: HashMap<PathBuf, Vec<Property>>,
}

impl Parse for GlaceMacro {
    fn parse(input: ParseStream) -> ParseResult<Self> {
        let attributes = input.call(Attribute::parse_outer)?;
        let path = validate_path_attribute(&attributes)?;
        let visibility: Option<Visibility> = input.parse().ok();
        input.parse::<Token![mod]>()?;
        let mod_name: Ident = input.parse()?;
        let mod_name = mod_name.to_string();

        let mut alias: Option<syn::Path> = None;
        let mut overrides = HashMap::new();

        let override_input;
        syn::braced!(override_input in input);
        let override_input = &override_input;

        if override_input.parse::<Token![use]>().is_ok() {
            let alias_override: syn::Path = override_input.parse()?;
            override_input.parse::<Token![as]>()?;
            let glace: Ident = override_input.parse()?;
            if glace != ident("glace") {
                panic!("The only valid crate alias is `use ... as glace;`");
            }
            override_input.parse::<Token![;]>()?;
            alias = Some(alias_override);
        }

        let overs: Punctuated<Override, Token![,]> = Punctuated::parse_terminated(override_input)?;
        for o in overs.into_iter() {
            overrides.insert(o.path, o.properties.into_iter().collect());
        }

        Ok(GlaceMacro {
            alias,
            visibility,
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
    WithCache,
    NoCache,
    WithConst,
    NoConst,
    WithIO,
    NoIO,

    Ignore,

    #[cfg(feature = "serde")]
    Single,

    #[cfg(feature = "serde")]
    Virtual,

    #[cfg(feature = "serde")]
    WithSerde,

    #[cfg(feature = "serde")]
    NoSerde,

    #[cfg(feature = "edres")]
    WithEdres,

    #[cfg(feature = "edres")]
    NoEdres,

    Mirror(PathBuf),
    Type(ParseType),

    #[cfg(feature = "serde")]
    Serde(Box<syn::Type>),

    // Transpose(TransposeType),
}

pub enum ParseType {
    #[cfg(feature = "image")]
    Image,

    #[cfg(feature = "serde_json")]
    Json,

    #[cfg(feature = "serde_toml")]
    Toml,

    #[cfg(feature = "serde_yaml")]
    Yaml,

    String,
    Binary,
    None,
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
            "WithCache" => Property::WithCache,
            "NoCache" => Property::NoCache,
            "WithConst" => Property::WithConst,
            "NoConst" => Property::NoConst,
            "WithIO" => Property::WithIO,
            "NoIO" => Property::NoIO,

            "Ignore" => Property::Ignore,

            #[cfg(feature = "serde")]
            "Single" => Property::Single,

            #[cfg(feature = "serde")]
            "Virtual" => Property::Virtual,

            "Mirror" => {
                input.parse::<Token![<]>()?;
                let path = input.parse::<syn::LitStr>()?.value().into();
                input.parse::<Token![>]>()?;
                Property::Mirror(path)
            }

            "Type" => {
                input.parse::<Token![<]>()?;
                let kind = input.parse::<syn::Ident>()?;
                let result = Property::Type(match kind.to_string().as_str() {
                    #[cfg(feature = "image")]
                    "Image" => ParseType::Image,

                    #[cfg(feature = "serde_json")]
                    "Json" => ParseType::Json,

                    #[cfg(feature = "serde_toml")]
                    "Toml" => ParseType::Toml,

                    #[cfg(feature = "serde_yaml")]
                    "Yaml" => ParseType::Yaml,

                    "String" => ParseType::String,
                    "Binary" => ParseType::Binary,
                    "None" => ParseType::None,
                    _ => Err(Error::new(kind.span(), "Unrecognized parse type"))?,
                });
                input.parse::<Token![>]>()?;
                result
            }

            #[cfg(feature = "serde")]
            "WithSerde" => Property::WithSerde,

            #[cfg(feature = "serde")]
            "NoSerde" => Property::NoSerde,

            #[cfg(feature = "edres")]
            "WithEdres" => Property::WithEdres,

            #[cfg(feature = "edres")]
            "NoEdres" => Property::NoEdres,

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
                let r#type = input.parse::<syn::Type>()?;
                input.parse::<Token![>]>()?;
                Property::Serde(Box::new(r#type))
            }
            _ => return Err(Error::new(ident.span(), "Unrecognized override for path")),
        };

        Ok(value)
    }
}
