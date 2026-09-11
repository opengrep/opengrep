// Comprehensive HOF test for Rust: Custom and built-in higher-order functions
// All of these should detect taint flow from source() to sink()

// ===== Custom HOF Functions =====

fn custom_map<T, F>(arr: &[T], callback: F) -> Vec<T>
where
    T: Clone,
    F: Fn(&T) -> T,
{
    let mut result = Vec::new();
    for item in arr {
        result.push(callback(item));
    }
    result
}

fn custom_for_each<T, F>(arr: &[T], callback: F)
where
    F: Fn(&T),
{
    for item in arr {
        callback(item);
    }
}

fn direct_call<F>(callback: F)
where
    F: Fn(String),
{
    callback(source());
}

// ===== Test Cases =====




// ===== Built-in iterator methods =====




// ===== Complex Example =====



// Stub functions


