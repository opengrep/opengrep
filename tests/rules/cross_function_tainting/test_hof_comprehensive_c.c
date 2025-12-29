// Comprehensive HOF test for C: Custom higher-order functions
// All of these should detect taint flow from source() to sink()

#include <stdio.h>
#include <stdlib.h>

// ===== Custom HOF Functions =====
// C uses function pointers for HOFs; no built-in HOF functions in standard library

void customForEach(char** arr, int size, void (*callback)(char*)) {
    for (int i = 0; i < size; i++) {
        callback(arr[i]);
    }
}

void directCall(void (*callback)(char*)) {
    callback(source());
}

// ===== Test Cases =====

void test_custom_foreach() {
    char* arr[] = {source()};
    customForEach(arr, 1, &sink_callback);
}

void sink_callback(char* x) {
    // ruleid: test-hof-taint
    sink(x);
}

void test_direct_call() {
    directCall(&sink_callback_direct);
}

void sink_callback_direct(char* x) {
    // ruleid: test-hof-taint
    sink(x);
}

// ===== Complex Example =====

char* getHistory(char* name, char* owner) {
    char* result = source();
    return result;
}

void test_original_example() {
    char* history = getHistory("name", "owner");
    char* arr[] = {history};
    customForEach(arr, 1, &sink_callback_complex);
}

void sink_callback_complex(char* node) {
    char* changes = node;
    // ruleid: test-hof-taint
    sink(changes);
}

// Stub functions
char* source() {
    return "tainted";
}

void sink(char* s) {
}

// ===== Top-level HOF Tests =====
// These test HOF callback detection at main scope level
// C doesn't allow function calls at global level, so we test in main

void toplevel_handler(char* x) {
    // ruleid: test-hof-taint
    sink(x);
}

int main() {
    test_custom_foreach();
    test_direct_call();
    test_original_example();

    // Top-level user-defined HOF with named callback
    char* toplevel_items[] = {source()};
    customForEach(toplevel_items, 1, &toplevel_handler);

    // Top-level direct call with function pointer
    directCall(&toplevel_handler);

    return 0;
}
