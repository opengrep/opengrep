// Comprehensive HOF test for Java: Custom higher-order functions
// All of these should detect taint flow from source() to sink()

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

class TestHOF {
    // ===== Custom HOF Functions =====

    static <T, R> List<R> customMap(List<T> arr, Function<T, R> callback) {
        List<R> result = new ArrayList<>();
        for (T item : arr) {
            result.add(callback.apply(item));
        }
        return result;
    }

    static <T> void customForEach(List<T> arr, Consumer<T> callback) {
        for (T item : arr) {
            callback.accept(item);
        }
    }

    static <T> void directCall(Consumer<T> callback, T value) {
        callback.accept(value);
    }

    // ===== Test Cases =====

    static void test_custom_map() {
        List<String> arr = Arrays.asList(source());
        customMap(arr, (x) -> {
            // ruleid: test-hof-taint
            sink(x);
            return x;
        });
    }

    static void test_custom_foreach() {
        List<String> arr = Arrays.asList(source());
        customForEach(arr, (x) -> {
            // ruleid: test-hof-taint
            sink(x);
        });
    }

    static void test_direct_call() {
        directCall((x) -> {
            // ruleid: test-hof-taint
            sink(x);
        }, source());
    }

    // ===== Built-in Stream methods =====

    static void test_builtin_map() {
        List<String> arr = Arrays.asList(source());
        arr.stream().map((x) -> {
            // ruleid: test-hof-taint
            sink(x);
            return x;
        }).collect(Collectors.toList());
    }

    static void test_builtin_filter() {
        List<String> arr = Arrays.asList(source());
        arr.stream().filter((x) -> {
            // ruleid: test-hof-taint
            sink(x);
            return true;
        }).collect(Collectors.toList());
    }

    static void test_builtin_forEach() {
        List<String> arr = Arrays.asList(source());
        arr.stream().forEach((x) -> {
            // ruleid: test-hof-taint
            sink(x);
        });
    }

    // ===== Complex Example =====

    static String getHistory(String name, String owner) {
        String result = source();
        return result;
    }

    static void test_original_example() {
        String history = getHistory("name", "owner");
        List<String> items = Arrays.asList(history).stream()
            .flatMap((node) -> {
                String changes = node;
                // ruleid: test-hof-taint
                sink(changes);
                return Stream.of(changes);
            })
            .collect(Collectors.toList());
    }

    // Stub methods
    static String source() { return "tainted"; }
    static void sink(String s) { }
}
