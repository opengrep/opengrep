// Test: Java collection models for taint propagation
// With taint_intrafile: findings should be detected

import java.util.HashMap;
import java.util.ArrayList;
import java.util.List;

public class CollectionModelsTest {

    // Test HashMap put/get
    public void testHashMap(String tainted) {
        HashMap<String, String> map = new HashMap<>();
        map.put("key", tainted);
        String value = map.get("key");
        // ruleid: java-collection-taint
        sink(value);
    }

    // Test List add/get
    public void testList(String tainted) {
        List<String> list = new ArrayList<>();
        list.add(tainted);
        String value = list.get(0);
        // ruleid: java-collection-taint
        sink(value);
    }

    // Test StringBuilder append (ToLval effect)
    public void testStringBuilder(String tainted) {
        StringBuilder sb = new StringBuilder();
        sb.append(tainted);
        // ruleid: java-collection-taint
        sink(sb);
    }

    // Test StringBuilder chained appends - currently NOT detected due to IL temporaries
    // The chained call creates temporaries, so ToLval taints _tmp, not sb
    public void testStringBuilderChained(String tainted) {
        StringBuilder sb = new StringBuilder();
        sb.append("prefix").append(tainted).append("suffix");
        // todoruleid: java-collection-taint
        sink(sb.toString());
    }

    // Test that clean data doesn't trigger
    public void testClean() {
        HashMap<String, String> map = new HashMap<>();
        map.put("key", "clean");
        String value = map.get("key");
        // ok: java-collection-taint
        sink(value);
    }

    private void sink(String data) {
        System.out.println(data);
    }
}
