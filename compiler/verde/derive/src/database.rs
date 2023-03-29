use std::num::TryFromIntError;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{spanned::Spanned, GenericArgument, Index, ItemStruct, PathArguments, Type, Visibility};

use crate::{Error, Result};

pub(crate) fn storage(input: ItemStruct) -> Result<TokenStream> {
	let Storage { vis, name, fields } = generate(input)?;
	let field_indices = (0..fields.len())
		.map(|x| {
			let x: u16 = x.try_into()?;
			Ok(Index::from(x as usize))
		})
		.collect::<std::result::Result<Vec<_>, _>>()
		.map_err(|_: TryFromIntError| Error::new(name.span(), "how do you have more than 65536 fields?"))?;

	Ok(quote! {
		#[derive(Default)]
		#[cfg_attr(feature = "serde", derive(::verde::serde::Serialize, ::verde::serde::Deserialize))]
		#vis struct #name(
			#(<#fields as ::verde::internal::Storable>::Storage),*
		);

		impl ::verde::internal::Storage for #name {
			fn init_routing(table: &mut ::verde::internal::storage::RouteBuilder) {
				#(table.add::<#fields>(#field_indices);)*
			}

			fn tracked_storage(&self, index: u16) -> Option<&dyn ::verde::internal::storage::ErasedTrackedStorage> {
				match index {
					#(#field_indices => <#fields as ::verde::internal::Storable>::tracked_storage(&self.#field_indices)),*,
					_ => panic!("invalid route index"),
				}
			}

			fn query_storage(&self, index: u16) -> Option<&dyn ::verde::internal::storage::ErasedQueryStorage> {
				match index {
					#(#field_indices => <#fields as ::verde::internal::Storable>::query_storage(&self.#field_indices)),*,
					_ => panic!("invalid route index"),
				}
			}

			fn pushable_storage(&self, index: u16) -> Option<&dyn ::verde::internal::storage::ErasedPushableStorage> {
				match index {
					#(#field_indices => <#fields as ::verde::internal::Storable>::pushable_storage(&self.#field_indices)),*,
					_ => panic!("invalid route index"),
				}
			}
		}

		#(
			impl ::verde::internal::StorageOf<#fields> for #name {
				fn storage_index(&self) -> u16 {
					#field_indices
				}
			}
		)*
	})
}

pub(crate) fn database(input: ItemStruct) -> Result<TokenStream> {
	let Storage { vis, name, fields } = generate(input)?;
	let ty_idents = fields
		.iter()
		.map(|field| ty_to_ident(field))
		.collect::<Result<Vec<_>>>()?;
	let field_names = ty_idents
		.iter()
		.map(|field| format_ident!("__verde_internal_storage_{}", field))
		.collect::<Vec<_>>();
	let field_indices = (1usize..)
		.take(fields.len())
		.map(|x| x.try_into())
		.collect::<std::result::Result<Vec<u16>, _>>()
		.map_err(|_| Error::new(name.span(), "how do you have more than 65536 fields?"))?;

	let derive = if cfg!(feature = "serde") {
		quote! { #[derive(::verde::serde::Serialize, ::verde::serde::Deserialize)] }
	} else {
		quote! {}
	};
	let skip = if cfg!(feature = "serde") {
		quote! { #[serde(skip, default = "__verde_internal_generate_routing_table")] }
	} else {
		quote! {}
	};

	Ok(quote! {
		fn __verde_internal_generate_routing_table() -> ::verde::internal::storage::RoutingTable {
			::verde::internal::storage::RoutingTable::generate_for_db::<#name>()
		}

		#derive
		#vis struct #name {
			#skip
			__verde_internal_routing_table: ::verde::internal::storage::RoutingTable,
			#(#field_names: #fields,)*
		}

		impl ::std::default::Default for #name {
			fn default() -> Self {
				Self {
					__verde_internal_routing_table: ::verde::internal::storage::RoutingTable::generate_for_db::<#name>(),
					#(#field_names: #fields::default()),*
				}
			}
		}

		impl ::verde::Db for #name {
			fn init_routing(table: &mut ::verde::internal::storage::RoutingTableBuilder) {
				#(<#fields as ::verde::internal::Storage>::init_routing(&mut table.start_route(#field_indices));)*
			}

			fn routing_table(&self) -> &::verde::internal::storage::RoutingTable {
				&self.__verde_internal_routing_table
			}

			fn storage_struct(&self, storage: u16) -> &dyn ::verde::internal::Storage {
				match storage {
					#(#field_indices => &self.#field_names),*,
					_ => panic!("invalid route storage"),
				}
			}
		}

		#(
			impl ::verde::internal::DbWith<#fields> for #name {
				fn storage_struct_index(&self) -> u16 {
					#field_indices
				}
			}
		)*
	})
}

struct Storage {
	vis: Visibility,
	name: Ident,
	fields: Vec<Type>,
}

fn generate(input: ItemStruct) -> Result<Storage> {
	if !input.generics.params.is_empty() {
		return Err(Error::new(
			input.generics.span(),
			"`storage` does not support generic types",
		));
	}

	let vis = input.vis;
	let name = input.ident;
	let fields = match input.fields {
		syn::Fields::Named(_) => {
			return Err(Error::new(
				input.fields.span(),
				"`storage` does not support named fields",
			));
		},
		syn::Fields::Unnamed(x) => x.unnamed.into_iter().map(|x| x.ty).collect(),
		syn::Fields::Unit => {
			return Err(Error::new(
				input.fields.span(),
				"`storage` does not support unit structs",
			));
		},
	};

	Ok(Storage { vis, name, fields })
}

fn ty_to_ident(x: &Type) -> Result<String> {
	let ret = match x {
		Type::Path(x) => x
			.path
			.segments
			.iter()
			.map(|x| {
				let ident = x.ident.to_string();
				match &x.arguments {
					PathArguments::None => Ok(ident),
					PathArguments::AngleBracketed(x) => Ok(x
						.args
						.iter()
						.map(|x| match x {
							GenericArgument::Type(x) => ty_to_ident(x),
							_ => Err(Error::new(x.span(), "`database` does not non-type generics")),
						})
						.collect::<Result<Vec<_>>>()?
						.join("_")),
					PathArguments::Parenthesized(_) => {
						Err(Error::new(x.span(), "`database` does not support function traits"))
					},
				}
			})
			.collect::<Result<Vec<_>>>()?
			.join("_"),
		_ => return Err(Error::new(x.span(), "`database` does not support non-path types")),
	};
	Ok(ret)
}
