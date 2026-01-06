// Test: Rust collection models for taint propagation
use std::collections::HashMap;

fn test_vec_push(tainted: String) {
    let mut vec = Vec::new();
    vec.push(tainted);
    // ruleid: rust-collection-taint
    sink(&vec);
}

fn test_vec_pop(tainted: String) {
    let mut vec = vec![tainted];
    let value = vec.pop();
    // ruleid: rust-collection-taint
    sink(&value);
}

fn test_hashmap_insert(tainted: String) {
    let mut map = HashMap::new();
    map.insert("key", tainted);
    let value = map.get("key");
    // ruleid: rust-collection-taint
    sink(&value);
}

fn test_vec_iter(tainted: String) {
    let vec = vec![tainted];
    let iter = vec.iter();
    // ruleid: rust-collection-taint
    sink(&iter);
}

fn sink<T>(data: &T) {
    println!("{:?}", std::any::type_name::<T>());
}
