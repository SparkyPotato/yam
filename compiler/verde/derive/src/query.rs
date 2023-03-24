use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
	spanned::Spanned,
	Block,
	FnArg,
	GenericParam,
	ItemFn,
	LifetimeParam,
	Pat,
	ReturnType,
	Type,
	TypeTuple,
	Visibility,
};

use crate::{Error, Result};

pub(crate) fn query(input: ItemFn) -> Result<TokenStream> {
	if input.sig.asyncness.is_some() {
		return Err(Error::new(input.sig.fn_token.span(), "query must not be `async`"));
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
		lifetimes,
		block,
	} = generate(&input)?;

	let input_type_name = format_ident!("__verde_internal_input_type_of_{}", name);

	let arg_types: Vec<_> = inputs.iter().map(|x| &x.ty).collect();
	let unref_arg_types: Vec<_> = arg_types
		.iter()
		.map(|x| {
			let ty = match x {
				Type::Reference(r) => r.elem.as_ref(),
				x => *x,
			};
			quote!(<#ty as ::std::borrow::ToOwned>)
		})
		.collect();

	let ctx_name = &ctx.name;
	let ctx_ty = match ctx.ty {
		Type::Reference(r) => r.elem,
		_ => return Err(Error::new(ctx.ty.span(), "context must be a `&Ctx`")),
	};
	let input_names: Vec<_> = inputs.iter().map(|x| &x.name).collect();

	let fn_ty = quote! { for<'__verde_internal_ctx_lifetime, #(#lifetimes,)*> fn(&'__verde_internal_ctx_lifetime #ctx_ty<'__verde_internal_ctx_lifetime>, #(#arg_types,)*) -> ::verde::Id<#ret_ty> };

	Ok(quote! {
		#[allow(non_camel_case_types)]
		#[derive(Copy, Clone)]
		#vis struct #name;

		#[allow(non_camel_case_types)]
		#[derive(Clone, PartialEq, Eq, Hash)]
		#vis struct #input_type_name {
			#(#input_names: #unref_arg_types::Owned,)*
		}

		impl ::std::ops::Deref for #name {
			type Target = #fn_ty;

			fn deref(&self) -> &Self::Target {
				fn inner<'__verde_internal_ctx_lifetime, #(#lifetimes)*>(#ctx_name: &'__verde_internal_ctx_lifetime #ctx_ty<'__verde_internal_ctx_lifetime>, #(#inputs,)*) -> ::verde::Id<#ret_ty> {
					let __verde_internal_query_input = #input_type_name {
						#(#input_names: #unref_arg_types::to_owned(&#input_names),)*
					};
					let __verde_internal_ctx = #ctx_name.start_query::<#name>(__verde_internal_query_input);

					let #ctx_name = &__verde_internal_ctx;

					__verde_internal_ctx.end_query::<#name>(|| #block)
				}

				const F: #fn_ty = inner;
				&F
			}
		}

		impl ::verde::internal::Query for #name {
			type Input = #input_type_name;
			type Output = #ret_ty;
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
	lifetimes: Vec<LifetimeParam>,
	block: Box<Block>,
}

fn generate(input: &ItemFn) -> Result<Query> {
	let vis = input.vis.clone();
	let name = input.sig.ident.clone();

	let lifetimes = input
		.sig
		.generics
		.params
		.iter()
		.map(|x| match x {
			GenericParam::Lifetime(l) => Ok(l.clone()),
			_ => Err(Error::new(x.span(), "query functions cannot be generic")),
		})
		.collect::<Result<Vec<_>>>()?;

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
		lifetimes,
		block,
	})
}
