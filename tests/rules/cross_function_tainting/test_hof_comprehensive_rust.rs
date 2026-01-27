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

fn test_custom_map() {
    let tainted = source();
    let arr = vec![tainted];
    custom_map(&arr, |x| {
        // ruleid: test-hof-taint
        sink(x);
        x.clone()
    });
}

fn test_custom_foreach() {
    let tainted = source();
    let arr = vec![tainted];
    custom_for_each(&arr, |x| {
        // ruleid: test-hof-taint
        sink(x);
    });
}

fn test_direct_call() {
    direct_call(|x| {
        // ruleid: test-hof-taint
        sink(&x);
    });
}

// ===== Built-in iterator methods =====

fn test_builtin_map() {
    let tainted = source();
    let arr = vec![tainted];
    arr.iter()
        .map(|x| {
            // ruleid: test-hof-taint
            sink(x);
            x
        })
        .collect::<Vec<_>>();
}

fn test_builtin_for_each() {
    let tainted = source();
    let arr = vec![tainted];
    arr.iter().for_each(|x| {
        // ruleid: test-hof-taint
        sink(x);
    });
}

fn test_builtin_filter() {
    let tainted = source();
    let arr = vec![tainted];
    arr.iter()
        .filter(|x| {
            // ruleid: test-hof-taint
            sink(x);
            true
        })
        .collect::<Vec<_>>();
}

// ===== Complex Example =====

fn get_history(name: &str, owner: &str) -> String {
    let result = source();
    result
}

fn test_original_example() {
    let history = get_history("name", "owner");
    let arr = vec![history];
    custom_for_each(&arr, |node| {
        let changes = node.clone();
        // ruleid: test-hof-taint
        sink(changes);
    });
}

// Stub functions
fn source() -> String {
    "tainted".to_string()
}

fn sink(_s: &String) {}

fn main() {
    test_custom_map();
    test_custom_foreach();
    test_direct_call();
    test_builtin_map();
    test_builtin_for_each();
    test_builtin_filter();
    test_original_example();
}
