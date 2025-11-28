extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_quote;
use syn::{
    Expr, Ident, LitInt, Token, bracketed, parenthesized, parse::Parse, parse_macro_input,
    token::Paren,
};

enum StackItem {
    Name(Ident),
    NameCounted(Ident, Expr),
    /// Amount of unused (and unnamed) stack items
    Unused(Expr),
}

struct StackEffect {
    pops: Vec<StackItem>,
    pushes: Vec<StackItem>,
}

struct Opcode {
    name: Ident,
    number: LitInt,
    stack_effect: StackEffect,
}

impl Parse for Opcode {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Example: LOAD_CONST = 100 ( -- constant)
        let name: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let number: LitInt = input.parse()?;

        let inner_stack_effect;

        parenthesized!(inner_stack_effect in input);

        let mut stack_effect = StackEffect {
            pops: vec![],
            pushes: vec![],
        };

        // Pops
        while inner_stack_effect.peek(Ident) {
            let name: Ident = inner_stack_effect.parse()?;

            stack_effect.pops.push(
                // This name is special, see the reference document on GitHub.
                if name == "unused" {
                    if inner_stack_effect.peek(syn::token::Bracket) {
                        let inner_bracket;
                        bracketed!(inner_bracket in inner_stack_effect);
                        let size: Expr = inner_bracket.parse()?;
                        StackItem::Unused(size)
                    } else {
                        StackItem::Unused(Expr::Lit(syn::ExprLit {
                            attrs: vec![],
                            lit: syn::Lit::Int(LitInt::new(
                                "1",
                                proc_macro::Span::call_site().into(),
                            )),
                        }))
                    }
                } else {
                    if inner_stack_effect.peek(syn::token::Bracket) {
                        let inner_bracket;
                        bracketed!(inner_bracket in inner_stack_effect);
                        let size: Expr = inner_bracket.parse()?;
                        StackItem::NameCounted(name, size)
                    } else {
                        StackItem::Name(name)
                    }
                },
            );

            if inner_stack_effect.parse::<Token![,]>().is_err() {
                break;
            }
        }

        inner_stack_effect.parse::<Token![-]>()?;
        inner_stack_effect.parse::<Token![-]>()?;

        while inner_stack_effect.peek(Ident) {
            let name: Ident = inner_stack_effect.parse()?;

            stack_effect.pushes.push(
                // This name is special, see the reference document on GitHub.
                if name == "unused" {
                    if inner_stack_effect.peek(syn::token::Bracket) {
                        let inner_bracket;
                        bracketed!(inner_bracket in inner_stack_effect);
                        let size: Expr = inner_bracket.parse()?;
                        StackItem::Unused(size)
                    } else {
                        StackItem::Unused(Expr::Lit(syn::ExprLit {
                            attrs: vec![],
                            lit: syn::Lit::Int(LitInt::new(
                                "1",
                                proc_macro::Span::call_site().into(),
                            )),
                        }))
                    }
                } else {
                    if inner_stack_effect.peek(syn::token::Bracket) {
                        let inner_bracket;
                        bracketed!(inner_bracket in inner_stack_effect);
                        let size: Expr = inner_bracket.parse()?;
                        StackItem::NameCounted(name, size)
                    } else {
                        StackItem::Name(name)
                    }
                },
            );

            if inner_stack_effect.parse::<Token![,]>().is_err() {
                break;
            }
        }

        Ok(Opcode {
            name,
            number,
            stack_effect,
        })
    }
}

struct Opcodes {
    opcodes: Vec<Opcode>,
}

impl Parse for Opcodes {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut opcodes = vec![];

        loop {
            opcodes.push(Opcode::parse(input)?);

            if input.parse::<Token![,]>().is_err() || input.is_empty() {
                break;
            }
        }

        Ok(Opcodes { opcodes })
    }
}

fn sum_items(items: &[StackItem]) -> Expr {
    if items.is_empty() {
        // 0 if empty
        Expr::Lit(syn::ExprLit {
            attrs: vec![],
            lit: syn::Lit::Int(LitInt::new("0", proc_macro::Span::call_site().into())),
        })
    } else {
        items
            .iter()
            .map(|p| match p {
                StackItem::Name(_) => Expr::Lit(syn::ExprLit {
                    attrs: vec![],
                    lit: syn::Lit::Int(LitInt::new("1", proc_macro::Span::call_site().into())),
                }),
                StackItem::NameCounted(_, size) => size.clone(),
                StackItem::Unused(size) => size.clone(),
            })
            .reduce(|left, right| {
                syn::Expr::Binary(syn::ExprBinary {
                    attrs: vec![],
                    left: Box::new(left),
                    op: syn::BinOp::Add(syn::token::Plus {
                        spans: [proc_macro::Span::call_site().into()],
                    }),
                    right: Box::new(right),
                })
            })
            .expect("Something is wrong with the format")
    }
}

#[proc_macro]
pub fn define_opcodes(input: TokenStream) -> TokenStream {
    let Opcodes { opcodes } = parse_macro_input!(input as Opcodes);

    let names: Vec<_> = opcodes.iter().map(|o| &o.name).collect();
    let numbers: Vec<_> = opcodes.iter().map(|o| &o.number).collect();

    let pops: Vec<_> = opcodes
        .iter()
        .map(|o| sum_items(&o.stack_effect.pops))
        .collect();

    let pushes: Vec<_> = opcodes
        .iter()
        .map(|o| sum_items(&o.stack_effect.pushes))
        .collect();

    let expanded = quote! {
        #[allow(non_camel_case_types)]
        #[allow(clippy::upper_case_acronyms)]
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum Opcode {
            #( #names ),*,
            INVALID_OPCODE(u8),
        }

        impl From<u8> for Opcode {
            fn from(value: u8) -> Self {
                match value {
                    #( #numbers => Opcode::#names, )*
                    _ => Opcode::INVALID_OPCODE(value),
                }
            }
        }

        impl From<Opcode> for u8 {
            fn from(value: Opcode) -> Self {
                match value {
                    #( Opcode::#names => #numbers , )*
                    Opcode::INVALID_OPCODE(value) => value,
                }
            }
        }

        impl From<(Opcode, u8)> for Instruction {
            fn from(value: (Opcode, u8)) -> Self {
                match value.0 {
                    #(
                        Opcode::#names => get_names!(@instruction #names, value.1),
                    )*
                    Opcode::INVALID_OPCODE(opcode) => {
                        if !cfg!(test) {
                            Instruction::InvalidOpcode((opcode, value.1))
                        } else {
                            panic!("Testing environment should not come across invalid opcodes")
                        }
                    },
                }
            }
        }

        impl Opcode {
            pub fn from_instruction(instruction: &Instruction) -> Self {
                match instruction {
                    #(
                        get_names!(@instruction #names) => Opcode::#names ,
                    )*
                    Instruction::InvalidOpcode((opcode, _)) => Opcode::INVALID_OPCODE(*opcode),
                }
            }
        }

        impl StackEffectTrait for Opcode {
            fn stack_effect(&self, oparg: u32, jump: Option<bool>) -> StackEffect {
                match &self {
                    #(
                        Opcode::#names => StackEffect { pops: #pops, pushes: #pushes },
                    )*
                }
            }
        }

        #[macro_export]
        macro_rules! get_names {
            (@instruction $variant:ident, $val:expr) => {
                paste::paste! { Instruction::[<$variant:camel>]($val) }
            };
            (@instruction $variant:ident) => {
                paste::paste! { Instruction::[<$variant:camel>](_) }
            };
        }
    };

    expanded.into()
}
