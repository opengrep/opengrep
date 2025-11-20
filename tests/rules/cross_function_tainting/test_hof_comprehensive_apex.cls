// HOF test for Apex: Custom higher-order functions using simple loops
// This tests taint flow through array iteration patterns

public class TestHOF {
    // Custom forEach that iterates and calls sink on each item
    public static void customForEach(List<String> arr) {
        for (String item : arr) {
        
           // ruleid: test-hof-taint
            sink(item);
        }
    }

    // Test case: taint flows through customForEach
    public static void test_custom_foreach() {
        List<String> arr = new List<String>{source()};
                customForEach(arr);
    }

    // Stub functions
    public static String source() {
        return 'tainted';
    }

    public static void sink(String s) {
    }
}
