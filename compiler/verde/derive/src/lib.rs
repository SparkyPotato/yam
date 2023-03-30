use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, ItemFn, ItemStruct};

mod database;
mod pushable;
mod query;
mod tracked;

type Result<T> = std::result::Result<T, Error>;
struct Error {
	span: proc_macro2::Span,
	message: String,
}

impl Error {
	fn new(span: proc_macro2::Span, message: impl ToString) -> Self {
		Self {
			span,
			message: message.to_string(),
		}
	}
}

impl ToTokens for Error {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let message = &self.message;
		tokens.extend(quote_spanned! { self.span => compile_error!(#message); });
	}
}

/// Allow a type to be tracked by the database.
///
/// This type must also implement `Eq`, and by extension, `PartialEq`.
///
/// A single field must be marked with `#[id]`, which will uniquely identify an instance of this type output
/// by a query. Different query functions may output equal IDs, but they will not interfere with each other. The ID must
/// implement `Eq`, `Hash`, and `Clone`.
#[proc_macro_derive(Tracked, attributes(id))]
pub fn tracked(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = parse_macro_input!(item as DeriveInput);
	match tracked::tracked(input) {
		Ok(x) => x,
		Err(e) => quote!(#e),
	}
	.into()
}

/// Allow a type to be interned in the database.
///
/// This type must implement `Clone`, `Eq`, and `Hash`.
#[proc_macro_derive(Interned)]
pub fn interned(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = parse_macro_input!(item as DeriveInput);
	let ty = input.ident;
	let (i, t, w) = input.generics.split_for_impl();
	(quote! {
		impl #i ::verde::Interned for #ty #t #w {}

		impl #i ::verde::internal::Storable for #ty #t #w {
			type Storage = ::verde::internal::storage::InternedStorage<Self>;

			const IS_PUSHABLE: bool = false;

			fn tracked_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedTrackedStorage> {
				None
			}

			fn query_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedQueryStorage> {
				None
			}

			fn pushable_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedPushableStorage> {
				None
			}

			fn interned_storage(store: &Self::Storage) -> Option<&dyn ::verde::internal::storage::ErasedInternedStorage> {
				Some(store)
			}
		}
	})
	.into()
}

/// Allow a type to be pushed into the database from queries.
#[proc_macro_derive(Pushable)]
pub fn pushable(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = parse_macro_input!(item as DeriveInput);
	match pushable::pushable(input) {
		Ok(x) => x,
		Err(e) => quote!(#e),
	}
	.into()
}

/// Generate a query.
///
/// The first argument of the function must be of type `&verde::Ctx`, and they must return a `Tracked` struct.
///
/// Arguments can be marked with `#[ignore]` to not be used to identify an execution of the query.
#[proc_macro_attribute]
pub fn query(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let attr = TokenStream::from(attr);
	if !attr.is_empty() {
		return quote_spanned! { attr.span() => compile_error!("`query` does not take any arguments"); }.into();
	}

	let input = parse_macro_input!(item as ItemFn);
	match query::query(input) {
		Ok(x) => x,
		Err(e) => quote!(#e),
	}
	.into()
}

/// Generate a storage struct.
///
/// Storage structs must be a tuple struct that contain the `Tracked`, `Pushable`, and `Interned` types, as well the
/// queries that to be stored into the database.
#[proc_macro_attribute]
pub fn storage(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let attr = TokenStream::from(attr);
	if !attr.is_empty() {
		return quote_spanned! { attr.span() => compile_error!("`storage` does not take any arguments"); }.into();
	}

	let input = parse_macro_input!(item as ItemStruct);
	match database::storage(input) {
		Ok(x) => x,
		Err(e) => quote!(#e),
	}
	.into()
}

/// Generate a database.
///
/// Databases must be a tuple struct that contain the `Storage` types.
#[proc_macro_attribute]
pub fn db(attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let attr = TokenStream::from(attr);
	if !attr.is_empty() {
		return quote_spanned! { attr.span() => compile_error!("`database` does not take any arguments"); }.into();
	}

	let input = parse_macro_input!(item as ItemStruct);
	match database::database(input) {
		Ok(x) => x,
		Err(e) => quote!(#e),
	}
	.into()
}
