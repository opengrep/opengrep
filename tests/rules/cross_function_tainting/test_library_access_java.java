// Field-sensitive taint through java.util.Map's .get / .getOrDefault
// / .put, type-gated on the receiver having a Map-family declared
// type. Non-Map receivers fall through to the generic call and
// taint flows through the callee's intrafile body.

import java.util.HashMap;
import java.util.Map;

class T {
    static void handlerGetPos() {
        Map<String, String> m = new HashMap<>();
        m.put("body", source());
        m.put("user", "safe");
        // ruleid: test-library-access-taint
        sink(m.get("body"));
    }

    static void handlerGetNeg() {
        Map<String, String> m = new HashMap<>();
        m.put("body", "safe");
        m.put("user", source());
        // ok: test-library-access-taint
        sink(m.get("body"));
    }

    static void handlerGetOrDefaultPos() {
        Map<String, String> m = new HashMap<>();
        m.put("body", source());
        // ruleid: test-library-access-taint
        sink(m.getOrDefault("body", "fallback"));
    }

    static void handlerGetOrDefaultTaintedDefault() {
        Map<String, String> m = new HashMap<>();
        m.put("body", "safe");
        // ruleid: test-library-access-taint
        sink(m.getOrDefault("body", source()));
    }
}

// Type-gating negative check: [MyMap] is not in [java.util.Map]'s
// family, so the [.get] rewrite must not trigger even though the
// method name matches. [MyMap#get] ignores its key argument and
// always returns [this.stored]; seeding [m.stored] with [source()]
// makes the intrafile call analyser carry taint through the opaque
// call and the sink fires. If the rewrite fired wrongly, [m.body]
// would project a clean cell via the shape layer (MyMap has no
// [body] field) and the finding would disappear.
class MyMap {
    String stored;
    String get(String key) { return this.stored; }
}

class T2 {
    static void handlerNonMapOpaqueTaintFlows() {
        MyMap m = new MyMap();
        m.stored = source();
        // ruleid: test-library-access-taint
        sink(m.get("body"));
    }
}
