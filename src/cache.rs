use std::{collections::HashMap, hash::Hash, sync::Arc, time::SystemTime};

use parking_lot::RwLock;

use crate::Asset;

type Entry<V> = (Arc<V>, Option<SystemTime>);

#[derive(Default)]
pub struct RwCache<K, V>
where
    K: Asset<Value = V> + Clone + Eq + Hash,
{
    backing: RwLock<HashMap<K, Entry<V>>>,
}

impl<K, V> RwCache<K, V>
where
    K: Asset<Value = V> + Clone + Eq + Hash,
{
    pub fn new() -> Self {
        RwCache {
            backing: Default::default(),
        }
    }

    pub fn get(&self, key: &K) -> Arc<V> {
        {
            let cache = self.backing.read();
            if let Some((result, _time)) = cache.get(key) {
                return Arc::clone(result);
            }
        }
        let data = Arc::new(key.value());
        {
            let mut cache = self.backing.write();
            cache.insert(key.clone(), (Arc::clone(&data), None));
        }
        data
    }

    pub fn get_updated(&self, key: &K) -> Arc<V> {
        let previous_entry = {
            let cache = self.backing.read();
            cache.get(key).map(|(data, time)| (Arc::clone(data), *time))
        };
        let previous_time = previous_entry.as_ref().and_then(|x| x.1);

        if let Some((updated_value, updated_time)) = key.value_modified(previous_time) {
            let updated_value = Arc::new(updated_value);
            {
                let mut cache = self.backing.write();
                cache.insert(
                    key.clone(),
                    (Arc::clone(&updated_value), Some(updated_time)),
                );
            }
            updated_value
        } else {
            previous_entry.unwrap().0
        }
    }

    pub fn reload(&self, key: &K) -> Arc<V> {
        let data = Arc::new(key.value());
        {
            let mut cache = self.backing.write();
            cache.insert(key.clone(), (Arc::clone(&data), None));
        }
        data
    }

    pub fn remove(&self, key: &K) {
        let mut cache = self.backing.write();
        cache.remove(key);
    }

    pub fn clear(&self) {
        let mut cache = self.backing.write();
        cache.clear();
    }
}
