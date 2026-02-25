extern crate proc_macro;
use heck::ToUpperCamelCase;
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Expr, Ident, LitInt, Token, bracketed, parenthesized, parse::Parse, parse_macro_input,
    spanned::Spanned,
};

#[derive(Clone)]
enum StackItem {
    Name(Ident),
    NameCounted(Ident, Expr),
    /// Amount of unused (and unnamed) stack items
    Unused(Expr),
}

#[derive(Clone)]
struct StackEffect {
    pops: Vec<StackItem>,
    pushes: Vec<StackItem>,
}

#[derive(Clone)]
struct Opcode {
    name: Ident,
    number: LitInt,
    stack_effect: Option<StackEffect>,
}

struct Exception {
    stack_effect: StackEffect,
}

impl Parse for Opcode {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Example: LOAD_CONST = 100 ( -- constant)
        // You may also use ( / ) to indicate this opcode has no stack description.
        let name: Ident = input.parse()?;
        input.parse::<Token![=]>()?;
        let number: LitInt = input.parse()?;

        let inner_stack_effect;

        parenthesized!(inner_stack_effect in input);

        let mut stack_effect = StackEffect {
            pops: vec![],
            pushes: vec![],
        };

        if inner_stack_effect.parse::<Token![/]>().is_ok() {
            // This opcode does not have a stack description
            return Ok(Opcode {
                name,
                number,
                stack_effect: None,
            });
        }

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
            stack_effect: Some(stack_effect),
        })
    }
}

impl Parse for Exception {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // Special parsing for custom exception specification
        // Example: *EXCEPTION ( -- lasti[if lasti {1} else {0}], exception)

        input.parse::<Token![*]>()?;
        syn::custom_keyword!(EXCEPTION);
        input.parse::<EXCEPTION>()?;

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

        Ok(Exception { stack_effect })
    }
}

struct Opcodes {
    opcodes: Vec<Opcode>,
    exception: Option<Exception>,
}

impl Parse for Opcodes {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut opcodes = vec![];
        let mut exception = None;

        loop {
            if input.peek(Token![*]) {
                exception = Some(Exception::parse(input)?);
            } else {
                opcodes.push(Opcode::parse(input)?);
            }

            if input.parse::<Token![,]>().is_err() || input.is_empty() {
                break;
            }
        }

        Ok(Opcodes { opcodes, exception })
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
    let Opcodes { opcodes, exception } = parse_macro_input!(input as Opcodes);

    let opcodes_with_stack: Vec<_> = opcodes
        .iter()
        .filter(|o| o.stack_effect.is_some())
        .collect();

    let names: Vec<_> = opcodes.iter().map(|o| &o.name).collect();
    let camel_names: Vec<Ident> = names
        .iter()
        .map(|ident| {
            let camel = ident.to_string().to_upper_camel_case();
            Ident::new(&camel, ident.span())
        })
        .collect();

    let names_with_stack: Vec<_> = opcodes_with_stack.iter().map(|o| &o.name).collect();

    let numbers: Vec<_> = opcodes.iter().map(|o| &o.number).collect();

    let pops: Vec<_> = opcodes_with_stack
        .iter()
        .map(|o| sum_items(&o.stack_effect.as_ref().unwrap().pops))
        .collect();

    let pushes: Vec<_> = opcodes_with_stack
        .iter()
        .map(|o| sum_items(&o.stack_effect.as_ref().unwrap().pushes))
        .collect();

    let mut expanded = quote! {
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
                        Opcode::#names => Instruction::#camel_names(value.1),
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
                        Instruction::#camel_names(_) => Opcode::#names ,
                    )*
                    Instruction::InvalidOpcode((opcode, _)) => Opcode::INVALID_OPCODE(*opcode),
                }
            }
        }

        impl StackEffectTrait for Opcode {
            fn stack_effect(&self, oparg: u32, jump: bool, calculate_max: bool) -> StackEffect {
                match &self {
                    #(
                        Opcode::#names_with_stack => StackEffect { pops: #pops, pushes: #pushes },
                    )*
                    Opcode::INVALID_OPCODE(_) => StackEffect { pops: 0, pushes: 0 },

                    _ => unimplemented!("stack_effect not implemented for {:?}", self),
                }
            }
        }
    };

    fn collect_stack_effect<'a, T>(
        stack_items: T,
        is_reverse: bool,
    ) -> Vec<proc_macro2::TokenStream>
    where
        T: DoubleEndedIterator<Item = &'a StackItem>,
    {
        let mut index = quote! { 0 };
        let mut fields = vec![];

        for pop in stack_items.rev() {
            match pop {
                StackItem::Name(name) => {
                    let name = name.to_string();
                    fields.push(quote! { StackItem { name: #name, count: 1, index: #index } });
                    index = quote! { (#index) + 1 };
                }
                StackItem::NameCounted(name, count) => {
                    let name = name.to_string();
                    let temp_index = if is_reverse {
                        quote! { (#index) - (#count).saturating_sub(1) }
                    } else {
                        quote! { #index }
                    };

                    fields.push(
                        quote! { StackItem { name: #name, count: #count, index: #temp_index  } },
                    );
                    index = quote! { (#index) + (#count) };
                }
                StackItem::Unused(count) => {
                    index = quote! { (#index) + (#count) };
                }
            }
        }

        fields
    }

    let mut input_sirs = vec![];
    let mut output_sirs = vec![];

    for (opcode, name) in opcodes.iter().zip(names) {
        let mut input_constructor_fields = vec![];
        let mut output_constructor_fields = vec![];

        if let Some(stack_effect) = &opcode.stack_effect {
            input_constructor_fields = collect_stack_effect(stack_effect.pops.iter(), false);

            input_constructor_fields.reverse();

            output_constructor_fields =
                collect_stack_effect(stack_effect.pushes.iter().rev(), true);
        }

        input_sirs.push(quote! { Opcode::#name => vec![
            #(
                #input_constructor_fields
            ),*
        ] });

        output_sirs.push(quote! { Opcode::#name => vec![
            #(
                #output_constructor_fields
            ),*
        ] });
    }

    let sir_exception = if let Some(exception) = exception {
        let mut input_fields = collect_stack_effect(exception.stack_effect.pops.iter(), false);

        input_fields.reverse();

        let output_fields = collect_stack_effect(exception.stack_effect.pushes.iter().rev(), true);

        quote! {
            #[derive(PartialEq, Debug, Clone)]
            pub struct SIRException {
                pub lasti: bool,
                pub input: Vec<StackItem>,
                pub output: Vec<StackItem>,
            }

            impl SIRException {
                pub fn new(lasti: bool, jump: bool) -> Self {
                    let input = vec![
                        #(
                            #input_fields
                        ),*
                    ];

                    let output = vec![
                        #(
                            #output_fields
                        ),*
                    ];

                    Self {
                        lasti,
                        input,
                        output,
                    }
                }
            }

            impl GenericSIRException for SIRException {
                type Opcode = Opcode;

                fn new(lasti: bool, jump: bool) -> Self {
                    SIRException::new(lasti, jump)
                }

                fn get_outputs(&self) -> &[StackItem] {
                    &self.output
                }

                fn get_inputs(&self) -> &[StackItem] {
                    &self.input
                }
            }

            impl std::fmt::Display for ExceptionCall<SIRNode, SIRException> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    let mut inputs = self
                        .stack_inputs
                        .iter()
                        .map(|input| format!("{}", input))
                        .collect::<Vec<_>>();

                    inputs.push(format!("{}", self.exception.lasti));

                    write!(f, "EXCEPTION({})", inputs.join(", "))
                }
            }
        }
    } else {
        quote! {#[derive(PartialEq, Debug, Clone)]
            /// This does not exist in versions without an exception_table
            pub struct SIRException {
            }

            impl std::fmt::Display for ExceptionCall<SIRNode, SIRException> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    unreachable!()
                }
            }

            impl GenericSIRException for SIRException {
                type Opcode = Opcode;

                fn new(lasti: bool, jump: bool) -> Self {
                    SIRException {}
                }

                fn get_outputs(&self) -> &[StackItem] {
                    &[]
                }

                fn get_inputs(&self) -> &[StackItem] {
                    &[]
                }
            }
        }
    };

    let sir = quote! {
        pub mod sir {
            use super::{Opcode};
            use crate::sir::{SIR, StackItem, SIRStatement, ExceptionCall, Call, SIRExpression, AuxVar};
            use crate::traits::{GenericSIRNode, SIROwned, GenericSIRException};


            #[derive(PartialEq, Debug, Clone)]
            pub struct SIRNode {
                pub opcode: Opcode,
                pub oparg: u32,
                pub input: Vec<StackItem>,
                pub output: Vec<StackItem>,
            }

            impl SIRNode {
                pub fn new(opcode: Opcode, oparg: u32, jump: bool) -> Self {
                    // This comes from the Python DSL where it is used to calculate the max stack usage possible. We intentionally disable it here.
                    let calculate_max = false;

                    let input = match opcode {
                        #(
                            #input_sirs
                        ),*,
                        Opcode::INVALID_OPCODE(_) => vec![],
                    };

                    let output = match opcode {
                        #(
                            #output_sirs
                        ),*,
                        Opcode::INVALID_OPCODE(_) => vec![],
                    };

                    Self {
                        opcode,
                        oparg,
                        input,
                        output,
                    }
                }
            }

            #sir_exception

            impl GenericSIRNode for SIRNode {
                type Opcode = Opcode;

                fn new(opcode: Self::Opcode, oparg: u32, jump: bool) -> Self {
                    SIRNode::new(opcode, oparg, jump)
                }

                fn get_outputs(&self) -> &[StackItem] {
                    &self.output
                }

                fn get_inputs(&self) -> &[StackItem] {
                    &self.input
                }
            }

            impl SIROwned<SIRNode, SIRException> for SIR<SIRNode, SIRException> {
                fn new(statements: Vec<SIRStatement<SIRNode, SIRException>>) -> Self {
                    SIR(statements)
                }
            }

            impl std::fmt::Display for SIR<SIRNode, SIRException> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    for statement in &self.0 {
                        match statement {
                            SIRStatement::Assignment(aux_var, call) => {
                                writeln!(f, "{} = {}", aux_var.name, call)?;
                            }
                            SIRStatement::TupleAssignment(aux_vars, call) => {
                                let vars = aux_vars.iter().map(|v| v.name.clone()).collect::<Vec<_>>().join(", ");
                                writeln!(f, "({}) = {}", vars, call)?;
                            }
                            SIRStatement::DisregardCall(call) => {
                                writeln!(f, "{}", call)?;
                            }
                        }
                    }
                    Ok(())
                }
            }

            impl std::fmt::Display for Call<SIRNode, SIRException> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    let mut inputs = self
                        .stack_inputs
                        .iter()
                        .map(|input| format!("{}", input))
                        .collect::<Vec<_>>();

                    inputs.push(format!("{}", self.node.oparg));

                    write!(f, "{:#?}({})", self.node.opcode, inputs.join(", "))
                }
            }

            impl std::fmt::Display for SIRExpression<SIRNode, SIRException> {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        SIRExpression::Call(call) => write!(f, "{}", call),
                        SIRExpression::Exception(exception_call) => write!(f, "{}", exception_call),
                        SIRExpression::AuxVar(aux_var) => write!(f, "{}", aux_var.name.clone()),
                        SIRExpression::PhiNode(phi) => write!(f, "phi({})", phi.iter().map(|v| &v.name).cloned().collect::<Vec<_>>().join(", ")),
                    }
                }
            }
        }
    };

    expanded.extend(sir);

    expanded.into()
}
