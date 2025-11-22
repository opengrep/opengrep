// HOF test for Apex: Custom higher-order functions
// This tests taint flow through a custom map function with a callback

public class TestHOF {
    // Custom map function that takes a callback
    public static List<String> customMap(List<String> arr, MapCallback callback) {
        List<String> result = new List<String>();
        for (String item : arr) {
            result.add(callback.apply(item));
        }
        return result;
    }

    // Test case: taint flows through customMap
    public static void test_custom_map() {
        List<String> arr = new List<String>{source()};
        customMap(arr, new MapCallback() {
            public String apply(String x) {
                // ruleid: test-hof-taint
                sink(x);
                return x;
            }
        });
    }

    // Stub functions
    public static String source() {
        return 'tainted';
    }

    public static void sink(String s) {
    }

    // Helper interface
    public interface MapCallback {
        String apply(String item);
    }
}
