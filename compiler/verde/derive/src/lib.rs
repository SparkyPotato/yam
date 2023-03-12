use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, ItemFn, ItemStruct};

mod database;
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

#[proc_macro_derive(Tracked, attributes(id))]
pub fn tracked(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = parse_macro_input!(item as DeriveInput);
	match tracked::tracked(input) {
		Ok(x) => x,
		Err(e) => quote!(#e),
	}
	.into()
}

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
