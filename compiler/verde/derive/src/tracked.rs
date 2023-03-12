use std::hash::{Hash, Hasher};

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
	spanned::Spanned,
	token::{Enum, Union},
	Data,
	DataEnum,
	DataStruct,
	DataUnion,
	DeriveInput,
	Ident,
	Type,
};

use crate::{Error, Result};

pub(crate) fn tracked(input: DeriveInput) -> Result<TokenStream> {
	if !input.generics.params.is_empty() {
		return Err(Error::new(
			input.generics.span(),
			"Tracked cannot be derived for generic types",
		));
	}

	match input.data {
		Data::Struct(s) => {
			let tracked = generate(&s)?;

			let (id_field, id_ty) = &tracked.fields[tracked.id];
			let ty = input.ident;

			Ok(quote! {
				impl ::verde::Tracked for #ty {
					type Id = #id_ty;

					fn id(&self) -> &Self::Id {
						&self.#id_field
					}
				}

				impl ::verde::TrackedOrQuery for #ty {
					type ToStore = ::verde::TrackedStorage<Self>;

					fn tracked_storage(store: &Self::ToStore) -> Option<&dyn ::verde::storage::ErasedTrackedStorage> {
						Some(store)
					}

					fn query_storage(store: &Self::ToStore) -> Option<&dyn ::verde::storage::ErasedQueryStorage> {
						None
					}
				}
			})
		},
		Data::Enum(DataEnum {
			enum_token: Enum { span },
			..
		})
		| Data::Union(DataUnion {
			union_token: Union { span },
			..
		}) => Err(Error::new(span, "Tracked cannot be derived for enums or unions")),
	}
}

enum Field {
	Ident(Ident),
	Tuple(usize),
}

impl Hash for Field {
	fn hash<H: Hasher>(&self, state: &mut H) {
		match self {
			Field::Ident(i) => i.hash(state),
			Field::Tuple(i) => i.hash(state),
		}
	}
}

impl ToTokens for Field {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		match self {
			Field::Ident(i) => i.to_tokens(tokens),
			Field::Tuple(i) => i.to_tokens(tokens),
		}
	}
}

struct TrackedStruct {
	fields: Vec<(Field, Type)>,
	id: usize,
}

fn generate(input: &DataStruct) -> Result<TrackedStruct> {
	let mut id = None;

	let fields = input
		.fields
		.iter()
		.enumerate()
		.map(|(i, f)| {
			if let Some(attr) = f.attrs.iter().find(|x| x.path.is_ident("id")) {
				if !attr.tokens.is_empty() {
					return Err(Error::new(
						attr.tokens.span(),
						"id attribute does not take any arguments",
					));
				}

				if id.is_some() {
					return Err(Error::new(f.span(), "Only a single field can be marked with `#[id]`"));
				}
				id = Some(i);
			}

			Ok((
				f.ident.clone().map(Field::Ident).unwrap_or(Field::Tuple(i)),
				f.ty.clone(),
			))
		})
		.collect::<Result<_>>()?;

	if let Some(id) = id {
		Ok(TrackedStruct { fields, id })
	} else {
		Err(Error::new(
			input.struct_token.span(),
			"There must be a field marked with `#[id]`",
		))
	}
}
