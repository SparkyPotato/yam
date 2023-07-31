use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, GenericParam};

use crate::Result;

pub(crate) fn pushable(input: DeriveInput) -> Result<TokenStream> {
	let name = input.ident;
	let (impl_, ty, where_) = input.generics.split_for_impl();
	let bounds: Vec<_> = where_
		.into_iter()
		.map(|x| {
			let pred = x.predicates.iter();
			quote! { #(#pred)* }
		})
		.chain(input.generics.params.iter().filter_map(|x| match x {
			GenericParam::Type(x) => {
				let ident = &x.ident;
				Some(quote! {
					#ident: ::std::clone::Clone + ::std::marker::Send + 'static
				})
			},
			GenericParam::Lifetime(_) => None,
			GenericParam::Const(_) => None,
		}))
		.collect();

	Ok(quote! {
		impl #impl_ ::verde::internal::Storable for #name #ty
		where #(#bounds)* {
			type Storage = ::verde::internal::storage::PushableStorage<Self>;

			fn tracked_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedTrackedStorage> {
				None
			}

			fn query_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedQueryStorage> {
				None
			}

			fn pushable_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedPushableStorage> {
				Some(store)
			}

			fn interned_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedInternedStorage> {
				None
			}
		}

		impl #impl_ ::verde::Pushable for #name #ty where #(#bounds)*  {}
	})
}
