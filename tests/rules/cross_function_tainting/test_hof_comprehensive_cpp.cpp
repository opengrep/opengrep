// Comprehensive HOF test for C++: Custom and built-in higher-order functions
// All of these should detect taint flow from source() to sink()

#include <vector>
#include <string>
#include <functional>
#include <algorithm>

// ===== Custom HOF Functions =====

// Manual loop implementation
template<typename T>
std::vector<T> customMap(const std::vector<T>& arr, std::function<T(T)> callback) {
    std::vector<T> result;
    for (const auto& item : arr) {
        result.push_back(callback(item));
    }
    return result;
}

// Delegates to built-in (tests ToSinkInCall propagation)
template<typename T>
std::vector<T> customMapBuiltin(const std::vector<T>& arr, std::function<T(T)> callback) {
    std::vector<T> result;
    std::transform(arr.begin(), arr.end(), std::back_inserter(result), callback);
    return result;
}

template<typename T>
void customForEach(const std::vector<T>& arr, std::function<void(T)> callback) {
    for (const auto& item : arr) {
        callback(item);
    }
}

template<typename T>
void directCall(std::function<void(T)> callback, T value) {
    callback(value);
}

// Stub functions
std::string source() {
    return "tainted";
}

void sink(std::string s) {
}

// ===== Test Cases =====

void test_custom_map() {
    std::vector<std::string> arr = {source()};
    customMap<std::string>(arr, [](std::string x) {
        // ruleid: test-hof-taint
        sink(x);
        return x;
    });
}

void test_custom_map_builtin() {
    std::vector<std::string> arr = {source()};
    customMapBuiltin<std::string>(arr, [](std::string x) {
        // ruleid: test-hof-taint
        sink(x);
        return x;
    });
}

void test_custom_foreach() {
    std::vector<std::string> arr = {source()};
    customForEach<std::string>(arr, [](std::string x) {
        // ruleid: test-hof-taint
        sink(x);
    });
}

void test_direct_call() {
    directCall<std::string>([](std::string x) {
        // ruleid: test-hof-taint
        sink(x);
    }, source());
}

// ===== Built-in algorithms =====

void test_builtin_for_each() {
    std::vector<std::string> arr = {source()};
    std::for_each(arr.begin(), arr.end(), [](std::string x) {
        // ruleid: test-hof-taint
        sink(x);
    });
}

void test_builtin_transform() {
    std::vector<std::string> arr = {source()};
    std::vector<std::string> result;
    std::transform(arr.begin(), arr.end(), std::back_inserter(result),
        [](std::string x) {
            // ruleid: test-hof-taint
            sink(x);
            return x;
        });
}

// ===== Complex Example =====

std::string getHistory(std::string name, std::string owner) {
    std::string result = source();
    return result;
}

void test_original_example() {
    std::string history = getHistory("name", "owner");
    std::vector<std::string> vec = {history};
    customForEach<std::string>(vec, [](std::string node) {
        std::string changes = node;
        // ruleid: test-hof-taint
        sink(changes);
    });
}

// ===== Top-level HOF Tests =====
// These test HOF callback detection at main scope level
// C++ doesn't allow function calls at global level, so we test in main

void toplevelHandler(std::string x) {
    // ruleid: test-hof-taint
    sink(x);
}

int main() {
    test_custom_map();
    test_custom_map_builtin();
    test_custom_foreach();
    test_direct_call();
    test_builtin_for_each();
    test_builtin_transform();
    test_original_example();

    // Top-level lambda callback
    // ruleid: test-hof-taint
    auto toplevelSink = [](std::string x) { sink(x); };
    toplevelSink(source());

    // Top-level method HOF (for_each with named callback)
    std::vector<std::string> toplevelItems = {source()};
    std::for_each(toplevelItems.begin(), toplevelItems.end(), toplevelHandler);

    // Top-level user-defined HOF
    customForEach<std::string>(toplevelItems, toplevelHandler);

    return 0;
}
