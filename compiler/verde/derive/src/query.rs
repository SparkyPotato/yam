use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
	spanned::Spanned,
	Block,
	FnArg,
	GenericParam,
	ItemFn,
	LifetimeParam,
	Meta,
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
	let query_inputs: Vec<_> = inputs.iter().filter(|x| !x.ignore).collect();
	let query_input_names: Vec<_> = query_inputs.iter().map(|x| &x.name).collect();
	let query_input_types: Vec<_> = query_inputs.iter().map(|x| &x.ty).collect();

	let ctx_name = &ctx.name;
	let ctx_ty = match ctx.ty {
		Type::Reference(r) => r.elem,
		_ => return Err(Error::new(ctx.ty.span(), "context must be a `&Ctx`")),
	};

	let derive = if cfg!(feature = "serde") {
		quote! {
			#[derive(::verde::serde::Serialize, ::verde::serde::Deserialize)]
		}
	} else {
		quote! {}
	};
	let fn_ty = quote! { for<#(#lifetimes,)*> fn(&#ctx_ty, #(#arg_types,)*) -> ::verde::Id<#ret_ty> };

	Ok(quote! {
		#[allow(non_camel_case_types)]
		#[derive(Copy, Clone)]
		#derive
		#vis struct #name;

		#[allow(non_camel_case_types)]
		#[derive(Clone, PartialEq, Eq, Hash)]
		#derive
		#vis struct #input_type_name {
			#(#query_input_names: #query_input_types,)*
		}

		impl ::std::ops::Deref for #name {
			type Target = #fn_ty;

			fn deref(&self) -> &Self::Target {
				fn inner<#(#lifetimes)*>(#ctx_name: &#ctx_ty, #(#inputs,)*) -> ::verde::Id<#ret_ty> {
					let __verde_internal_query_input = #input_type_name {
						#(#query_input_names: ::std::clone::Clone::clone(&#query_input_names),)*
					};
					let __verde_internal_ctx = #ctx_name.start_query::<#name>(__verde_internal_query_input);
					let #ctx_name = &__verde_internal_ctx;
					__verde_internal_ctx.end_query::<#name>(move || #block)
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
	ignore: bool,
}

impl ToTokens for Arg {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let Arg { name, ty, .. } = self;
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
				ignore: {
					let mut iter = pat.attrs.iter();
					let x = iter
						.next()
						.map(|x| matches!(&x.meta, Meta::Path(p) if p.is_ident("ignore")))
						.unwrap_or(false);
					if iter.next().is_some() {
						return Err(Error::new(x.span(), "query arguments must have at most one attribute"));
					}
					x
				},
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
