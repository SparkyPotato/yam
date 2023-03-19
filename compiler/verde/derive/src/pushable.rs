use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::Result;

pub(crate) fn pushable(input: DeriveInput) -> Result<TokenStream> {
	let name = input.ident;
	let (impl_, ty, where_) = input.generics.split_for_impl();

	Ok(quote! {
		impl #impl_ ::verde::internal::Storable for #name #ty #where_ {
			type Storage = ::verde::internal::storage::PushableStorage<Self>;

			const IS_PUSHABLE: bool = true;

			fn tracked_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedTrackedStorage> {
				None
			}

			fn query_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedQueryStorage> {
				None
			}

			fn pushable_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedPushableStorage> {
				Some(store)
			}
		}

		impl #impl_ ::verde::Pushable for #name #ty #where_ {}
	})
}
