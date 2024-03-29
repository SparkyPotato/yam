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
	Meta,
	Type,
};

use crate::{Error, Result};

pub(crate) fn tracked(input: DeriveInput) -> Result<TokenStream> {
	match input.data {
		Data::Struct(s) => {
			let (id_field, id_ty) = generate(&s)?;
			let (generics, ty_generics, where_clause) = input.generics.split_for_impl();
			let ty = input.ident;

			Ok(quote! {
				impl #generics ::verde::Tracked for #ty #ty_generics #where_clause {
					type Id = #id_ty;

					fn id(&self) -> &Self::Id {
						&self.#id_field
					}
				}

				impl #generics ::verde::internal::Storable for #ty #ty_generics #where_clause {
					type Storage = ::verde::internal::storage::TrackedStorage<Self>;

					fn tracked_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedTrackedStorage> {
						Some(store)
					}

					fn query_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedQueryStorage> {
						None
					}

					fn pushable_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedPushableStorage> {
						None
					}

					fn interned_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedInternedStorage> {
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

fn generate(input: &DataStruct) -> Result<(Field, Type)> {
	let mut id = None;

	for (i, f) in input.fields.iter().enumerate() {
		if f.attrs
			.iter()
			.any(|x| matches!(&x.meta, Meta::Path(p) if p.is_ident("id")))
		{
			if id.is_some() {
				return Err(Error::new(f.span(), "Only a single field can be marked with `#[id]`"));
			}
			id = Some((
				f.ident.clone().map(Field::Ident).unwrap_or(Field::Tuple(i)),
				f.ty.clone(),
			));
		}
	}

	id.ok_or_else(|| Error::new(input.struct_token.span(), "There must be a field marked with `#[id]`"))
}
