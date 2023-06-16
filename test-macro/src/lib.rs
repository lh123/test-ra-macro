use proc_macro::TokenStream;
use quote::ToTokens;
use syn::visit_mut::VisitMut;

#[proc_macro_attribute]
pub fn test_macro(args: TokenStream, input: TokenStream) -> TokenStream {
    test_impl(args.into(), input.into()).into()
}

struct ProtectMacroAttr {
    display_name: syn::LitStr,
}

impl syn::parse::Parse for ProtectMacroAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let display_name = input.parse()?;
        Ok(ProtectMacroAttr { display_name })
    }
}

fn test_impl(
    args: proc_macro2::TokenStream,
    input: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let ProtectMacroAttr { display_name } = match syn::parse2(args) {
        Ok(args) => args,
        Err(err) => return err.to_compile_error().into(),
    };

    let mut function: syn::ItemFn = match syn::parse2(input.clone()) {
        Ok(function) => function,
        Err(err) => return make_err_token_stream(input.clone(), err),
    };

    let mut protected_function_block = function.block.clone();
    let protect_block_label = syn::Lifetime::new("'block", proc_macro2::Span::call_site());
    let mut block_rewriter = RewriteReturnToBreak::new(protect_block_label.clone());
    block_rewriter.visit_block_mut(&mut protected_function_block);
    if let Some(err) = block_rewriter.err {
        return make_err_token_stream(input.clone(), err);
    }

    let function_body: syn::Block = syn::parse_quote! {
        {
            println!(#display_name);
            let result = #protect_block_label: #protected_function_block;
            result
        }
    };
    function.block = Box::new(function_body);
    println!("{}", function.to_token_stream());
    quote::quote! {
        #[inline(never)]
        #function
    }
    .into()
}

fn make_err_token_stream(
    raw: proc_macro2::TokenStream,
    err: syn::Error,
) -> proc_macro2::TokenStream {
    let raw = proc_macro2::TokenStream::from(raw);
    let err = err.to_compile_error();
    quote::quote! {
        #raw
        #err
    }
    .into()
}

struct RewriteReturnToBreak {
    break_label: syn::Lifetime,
    is_top_level: bool,
    err: Option<syn::Error>,
}

impl RewriteReturnToBreak {
    fn new(break_label: syn::Lifetime) -> RewriteReturnToBreak {
        RewriteReturnToBreak {
            break_label,
            is_top_level: true,
            err: None,
        }
    }
}

impl syn::visit_mut::VisitMut for RewriteReturnToBreak {
    fn visit_expr_closure_mut(&mut self, i: &mut syn::ExprClosure) {
        let is_top_level = self.is_top_level;
        self.is_top_level = false;
        syn::visit_mut::visit_expr_closure_mut(self, i);
        self.is_top_level = is_top_level;
    }

    fn visit_item_fn_mut(&mut self, i: &mut syn::ItemFn) {
        let is_top_level = self.is_top_level;
        self.is_top_level = false;
        syn::visit_mut::visit_item_fn_mut(self, i);
        self.is_top_level = is_top_level;
    }

    fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
        if self.is_top_level {
            match expr {
                syn::Expr::Return(return_expr) => {
                    let b_expr = &return_expr.expr.as_mut().map(|expr| {
                        syn::visit_mut::visit_expr_mut(self, &mut *expr);
                        expr
                    });
                    let b_label = &self.break_label;
                    let break_expr: syn::ExprBreak = syn::parse_quote! {break #b_label #b_expr};
                    *expr = syn::Expr::Break(break_expr)
                }
                syn::Expr::Try(try_expr) => {
                    let inner_expr = &mut *try_expr.expr;
                    syn::visit_mut::visit_expr_mut(self, inner_expr);
                    let b_label = &self.break_label;
                    *expr = syn::Expr::Match(syn::parse_quote! {
                        match #inner_expr {
                            Ok(val) => val,
                            Err(err) => break #b_label Err(From::from(err)),
                        }
                    });
                }
                _ => syn::visit_mut::visit_expr_mut(self, expr),
            };
        }
    }
}
