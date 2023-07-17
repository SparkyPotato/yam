use verde::storage;

pub mod index;
pub mod module;

// TODO: Packages and @autoimport.

#[storage]
pub struct Storage(index::PublicIndex, index::PrivateIndex);
