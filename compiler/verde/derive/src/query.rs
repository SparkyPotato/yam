use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{spanned::Spanned, Block, FnArg, ItemFn, Pat, ReturnType, Type, TypeTuple, Visibility};

use crate::{Error, Result};

pub(crate) fn query(input: ItemFn) -> Result<TokenStream> {
	if input.sig.asyncness.is_none() {
		return Err(Error::new(input.sig.fn_token.span(), "query must be `async`"));
	}
	if !input.sig.generics.params.is_empty() {
		return Err(Error::new(
			input.sig.generics.span(),
			"query functions cannot be generic",
		));
	}

	let ret_ty = match &input.sig.output {
		ReturnType::Type(_, ty) if !matches!(ty.as_ref(), Type::Tuple(TypeTuple { elems, .. }) if elems.is_empty()) => {
			quote!(#ty)
		},
		_ => {
			return Err(Error::new(input.sig.output.span(), "query must return a value"));
		},
	};

	let Query {
		vis,
		name,
		ctx,
		inputs,
		block,
	} = generate(&input)?;

	let fn_type_name = format_ident!("__verde_internal_type_of_{}", name);
	let fut_type_name = format_ident!("__verde_internal_future_type_of_{}", name);
	let input_type_name = format_ident!("__verde_internal_input_type_of_{}", name);

	let arg_types = inputs.iter().map(|x| &x.ty);

	let ctx_name = &ctx.name;
	let input_names = inputs.iter().map(|x| &x.name);

	Ok(quote! {
		#[allow(non_camel_case_types)]
		#[derive(Copy, Clone)]
		#vis struct #name;
		#[allow(non_camel_case_types)]
		type #fut_type_name = impl ::std::future::Future<Output = ::verde::Id<#ret_ty>> + ::std::marker::Send;
		#[allow(non_camel_case_types)]
		type #fn_type_name = impl for<'a> ::std::ops::Fn(&'a (dyn ::verde::Db + 'a), #(#arg_types,)*) -> #fut_type_name;

		#[allow(non_camel_case_types)]
		#[derive(Clone, PartialEq, Eq, Hash)]
		struct #input_type_name {
			#(#inputs,)*
		}

		impl ::std::ops::Deref for #name {
			type Target = #fn_type_name;
			fn deref(&self) -> &Self::Target {
				fn inner(#ctx, #(#inputs,)*) -> #fut_type_name {
					let __verde_internal_parent_ctx_wrapper = ::verde::DbWrapper(unsafe { ::std::mem::transmute(#ctx_name) });
					async move {
						let __verde_internal_query_input = #input_type_name {
							#(#input_names: ::std::clone::Clone::clone(&#input_names),)*
						};
						let __verde_internal_parent_ctx = unsafe { __verde_internal_parent_ctx_wrapper.to_ref() };
						let __verde_internal_ctx = __verde_internal_parent_ctx.start_query::<#name>(__verde_internal_query_input).await;

						let out = async {
							let #ctx_name = &__verde_internal_ctx as &dyn ::verde::Db;
							#block
						};

						__verde_internal_parent_ctx.end_query::<#name, _>(&__verde_internal_ctx, out).await
					}
				}

				&inner
			}
		}

		impl ::verde::internal::Query for #name {
			type Input = #input_type_name;
			type Output = #ret_ty;
			type Future = #fut_type_name;
		}

		impl ::verde::internal::Storable for #name {
			type Storage = ::verde::internal::storage::QueryStorage<Self>;
			
			const IS_PUSHABLE: bool = false;

			fn tracked_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedTrackedStorage> {
				None
			}

			fn query_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedQueryStorage> {
				Some(store)
			}

			fn pushable_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedPushableStorage> {
				None
			}
		}
	})
}

#[derive(Hash)]
struct Arg {
	name: Ident,
	ty: Type,
}

impl ToTokens for Arg {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let Arg { name, ty } = self;
		tokens.extend(quote!(#name: #ty));
	}
}

struct Query {
	vis: Visibility,
	name: Ident,
	ctx: Arg,
	inputs: Vec<Arg>,
	block: Box<Block>,
}

fn generate(input: &ItemFn) -> Result<Query> {
	let vis = input.vis.clone();
	let name = input.sig.ident.clone();

	let mut args = input.sig.inputs.iter().map(|x| match x {
		FnArg::Receiver(_) => Err(Error::new(x.span(), "query functions cannot take `self`")),
		FnArg::Typed(pat) => match *pat.pat {
			Pat::Ident(ref ident) => Ok(Arg {
				name: ident.ident.clone(),
				ty: *pat.ty.clone(),
			}),
			_ => Err(Error::new(pat.pat.span(), "query arguments must be `ident` patterns")),
		},
	});

	let ctx = args.next().ok_or_else(|| {
		Error::new(
			input.sig.fn_token.span(),
			"query functions must take a `&dyn Db` as their first argument",
		)
	})??;
	let inputs = args.collect::<Result<_>>()?;
	let block = input.block.clone();

	Ok(Query {
		vis,
		name,
		ctx,
		inputs,
		block,
	})
}
