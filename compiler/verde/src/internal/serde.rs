//! TODO: Don't need this

use std::{fmt::Formatter, marker::PhantomData, sync::atomic::AtomicU64};

use parking_lot::{Mutex, RwLock};
use serde::{
	ser::{SerializeStruct, SerializeTuple},
	Deserialize,
	Deserializer,
	Serialize,
	Serializer,
};

use crate::{
	internal::{
		storage::{
			DashMap,
			ErasedId,
			OutputId,
			PushableStorage,
			QueryData,
			QueryStorage,
			Route,
			Slot,
			TrackedIdent,
			TrackedStorage,
		},
		Query,
	},
	Id,
	Tracked,
};

pub trait Serde {
	type Serializable: Serialize + for<'de> Deserialize<'de>;

	fn to_serializable(self) -> Self::Serializable;

	fn from_serializable(serializable: Self::Serializable) -> Self;
}

impl Serialize for Route {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		let mut tuple = serializer.serialize_tuple(2)?;
		tuple.serialize_element(&self.storage)?;
		tuple.serialize_element(&self.index)?;
		tuple.end()
	}
}

impl<'de> Deserialize<'de> for Route {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		struct RouteVisitor;

		impl<'de> serde::de::Visitor<'de> for RouteVisitor {
			type Value = Route;

			fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
				formatter.write_str("a tuple of two u16s")
			}

			fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
			where
				A: serde::de::SeqAccess<'de>,
			{
				let storage = seq
					.next_element()?
					.ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;
				let index = seq
					.next_element()?
					.ok_or_else(|| serde::de::Error::invalid_length(1, &self))?;
				Ok(Route { storage, index })
			}
		}

		deserializer.deserialize_tuple(2, RouteVisitor)
	}
}

impl Serialize for ErasedId {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		let mut tuple = serializer.serialize_tuple(2)?;
		tuple.serialize_element(&self.route)?;
		tuple.serialize_element(&self.index)?;
		tuple.end()
	}
}

impl<'de> Deserialize<'de> for ErasedId {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		struct ErasedIdVisitor;

		impl<'de> serde::de::Visitor<'de> for ErasedIdVisitor {
			type Value = ErasedId;

			fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
				formatter.write_str("a tuple of a `Route` and u32")
			}

			fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
			where
				A: serde::de::SeqAccess<'de>,
			{
				let route = seq
					.next_element()?
					.ok_or_else(|| serde::de::Error::invalid_length(0, &self))?;
				let index = seq
					.next_element()?
					.ok_or_else(|| serde::de::Error::invalid_length(1, &self))?;
				Ok(ErasedId { route, index })
			}
		}

		deserializer.deserialize_tuple(2, ErasedIdVisitor)
	}
}

impl<T> Serialize for Id<T> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> { self.inner.serialize(serializer) }
}

impl<'de, T> Deserialize<'de> for Id<T> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		Ok(Id {
			inner: Deserialize::deserialize(deserializer)?,
			_phantom: PhantomData,
		})
	}
}

#[derive(Serialize, Deserialize)]
pub struct SerializablePushableStorage<T> {
	map: DashMap<Route, Vec<Vec<T>>>,
}

impl<T> Serde for PushableStorage<T>
where
	T: Serialize + for<'de> Deserialize<'de>,
{
	type Serializable = SerializablePushableStorage<T>;

	fn to_serializable(self) -> Self::Serializable {
		SerializablePushableStorage {
			map: self
				.map
				.into_iter()
				.map(|(k, v)| (k, v.into_iter().map(|x| x.into_inner()).collect()))
				.collect(),
		}
	}

	fn from_serializable(serializable: Self::Serializable) -> Self {
		PushableStorage {
			map: serializable
				.map
				.into_iter()
				.map(|(k, v)| (k, v.into_iter().map(Mutex::new).collect()))
				.collect(),
		}
	}
}

impl<T: Query> Serialize for QueryData<T> {
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		let mut s = serializer.serialize_struct("QueryData", 2)?;
		s.serialize_field("dependencies", &self.dependencies)?;
		s.serialize_field(
			"output",
			&self.output.map(|x| x.get()).unwrap_or(Id {
				inner: ErasedId {
					route: Route { storage: 0, index: 0 },
					index: 0,
				},
				_phantom: PhantomData,
			}),
		)?;
		s.end()
	}
}

impl<'de, T: Query> Deserialize<'de> for QueryData<T> {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		struct QueryDataVisitor<T: Query>(PhantomData<T>);

		impl<'de, T: Query> serde::de::Visitor<'de> for QueryDataVisitor<T> {
			type Value = QueryData<T>;

			fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
				formatter.write_str("a struct with two fields: `dependencies` and `output`")
			}

			fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
			where
				A: serde::de::MapAccess<'de>,
			{
				let mut dependencies = None;
				let mut output = None;
				while let Some(key) = map.next_key()? {
					match key {
						"dependencies" => {
							if dependencies.is_some() {
								return Err(serde::de::Error::duplicate_field("dependencies"));
							}
							dependencies = Some(map.next_value()?);
						},
						"output" => {
							if output.is_some() {
								return Err(serde::de::Error::duplicate_field("output"));
							}
							output = Some(map.next_value()?);
						},
						_ => {
							let _: serde::de::IgnoredAny = map.next_value()?;
						},
					}
				}
				let dependencies = dependencies.ok_or_else(|| serde::de::Error::missing_field("dependencies"))?;
				let output: Id<_> = output.ok_or_else(|| serde::de::Error::missing_field("output"))?;
				Ok(QueryData {
					dependencies,
					output: if output.inner.route.storage != 0 {
						Some(OutputId::new(output))
					} else {
						None
					},
				})
			}
		}

		deserializer.deserialize_struct("QueryData", &["dependencies", "output"], QueryDataVisitor(PhantomData))
	}
}

#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "T::Input: Serialize"))]
#[serde(bound(deserialize = "T::Input: Deserialize<'de>"))]
pub struct SerializableQueryStorage<T: Query>
where
	T::Input: Serialize,
{
	map: DashMap<T::Input, u32>,
	values: Vec<QueryData<T>>,
}

impl<T: Query> Serde for QueryStorage<T>
where
	T::Input: Serialize + for<'de> Deserialize<'de>,
{
	type Serializable = SerializableQueryStorage<T>;

	fn to_serializable(self) -> Self::Serializable
	where
		T::Input: Serialize,
	{
		SerializableQueryStorage {
			map: self.map,
			values: self.values.into_inner().into_iter().map(|x| x.into_inner()).collect(),
		}
	}

	fn from_serializable(serializable: Self::Serializable) -> Self {
		QueryStorage {
			map: serializable.map,
			values: RwLock::new(serializable.values.into_iter().map(Mutex::new).collect()),
		}
	}
}

impl<T: Tracked> Serialize for TrackedIdent<T>
where
	T::Id: Serialize,
{
	fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
		let mut s = serializer.serialize_struct("TrackedIdent", 2)?;
		s.serialize_field("id", &self.id)?;
		s.serialize_field("query", &self.query)?;
		s.end()
	}
}

impl<'de, T: Tracked> Deserialize<'de> for TrackedIdent<T>
where
	T::Id: Deserialize<'de>,
{
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		struct TrackedIdentVisitor<T: Tracked>(PhantomData<T>);

		impl<'de, T: Tracked> serde::de::Visitor<'de> for TrackedIdentVisitor<T>
		where
			T::Id: Deserialize<'de>,
		{
			type Value = TrackedIdent<T>;

			fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
				formatter.write_str("a struct with two fields: `id` and `query`")
			}

			fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
			where
				A: serde::de::MapAccess<'de>,
			{
				let mut id = None;
				let mut query = None;
				while let Some(key) = map.next_key()? {
					match key {
						"id" => {
							if id.is_some() {
								return Err(serde::de::Error::duplicate_field("id"));
							}
							id = Some(map.next_value()?);
						},
						"query" => {
							if query.is_some() {
								return Err(serde::de::Error::duplicate_field("query"));
							}
							query = Some(map.next_value()?);
						},
						_ => {
							let _: serde::de::IgnoredAny = map.next_value()?;
						},
					}
				}
				let id = id.ok_or_else(|| serde::de::Error::missing_field("id"))?;
				let query = query.ok_or_else(|| serde::de::Error::missing_field("query"))?;
				Ok(TrackedIdent { id, query })
			}
		}

		deserializer.deserialize_struct("TrackedIdent", &["id", "query"], TrackedIdentVisitor(PhantomData))
	}
}

#[derive(Serialize, Deserialize)]
pub struct SerializableSlot<T> {
	value: T,
	generation: u64,
}

impl<T> Serde for Slot<T>
where
	T: Serialize + for<'de> Deserialize<'de>,
{
	type Serializable = SerializableSlot<T>;

	fn to_serializable(self) -> SerializableSlot<T> {
		SerializableSlot {
			value: self.value.into_inner(),
			generation: self.generation.into_inner(),
		}
	}

	fn from_serializable(serializable: SerializableSlot<T>) -> Self {
		Slot {
			value: RwLock::new(serializable.value),
			generation: AtomicU64::new(serializable.generation),
		}
	}
}

#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "T::Id: Serialize, T: Serialize"))]
#[serde(bound(deserialize = "T::Id: Deserialize<'de>, T: Deserialize<'de>"))]
pub struct SerializableTrackedStorage<T: Tracked> {
	map: DashMap<TrackedIdent<T>, u32>,
	values: Vec<SerializableSlot<T>>,
}

impl<T: Serialize + Tracked> Serde for TrackedStorage<T>
where
	T::Id: Serialize + for<'de> Deserialize<'de>,
	T: Serialize + for<'de> Deserialize<'de>,
{
	type Serializable = SerializableTrackedStorage<T>;

	fn to_serializable(self) -> Self::Serializable {
		SerializableTrackedStorage {
			map: self.map,
			values: self
				.values
				.into_inner()
				.into_iter()
				.map(Serde::to_serializable)
				.collect(),
		}
	}

	fn from_serializable(serializable: Self::Serializable) -> Self {
		TrackedStorage {
			map: serializable.map,
			values: RwLock::new(serializable.values.into_iter().map(Serde::from_serializable).collect()),
		}
	}
}
