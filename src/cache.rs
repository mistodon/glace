use std::collections::HashMap;
use std::hash::Hash;
use std::sync::{Arc, RwLock};

use crate::Asset;

pub struct RwCache<K, V>
where
    K: Asset<V> + Clone + Eq + Hash,
{
    backing: RwLock<HashMap<K, Arc<V>>>,
}

impl<K, V> RwCache<K, V>
where
    K: Asset<V> + Clone + Eq + Hash,
{
    // TODO: get_updated (reload if modified)

    pub fn get(&self, key: &K) -> Arc<V> {
        {
            let cache = self.backing.read().unwrap();
            if let Some(result) = cache.get(key) {
                return Arc::clone(result);
            }
        }
        let data = Arc::new(key.value());
        {
            let mut cache = self.backing.write().unwrap();
            cache.insert(key.clone(), Arc::clone(&data));
        }
        data
    }

    pub fn reload(&self, key: &K) -> Arc<V> {
        let data = Arc::new(key.value());
        {
            let mut cache = self.backing.write().unwrap();
            cache.insert(key.clone(), Arc::clone(&data));
        }
        data
    }

    pub fn remove(&self, key: &K) {
        let mut cache = self.backing.write().unwrap();
        cache.remove(key);
    }

    pub fn clear(&self) {
        let mut cache = self.backing.write().unwrap();
        cache.clear();
    }
}
