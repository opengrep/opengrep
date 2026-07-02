// Field-sensitive taint through java.util.Map's .get / .getOrDefault
// / .put, type-gated on the receiver having a Map-family declared
// type. Non-Map receivers fall through to the generic call and
// taint flows through the callee's intrafile body.

import java.util.HashMap;
import java.util.Map;


// Type-gating negative check: [MyMap] is not in [java.util.Map]'s
// family, so the [.get] rewrite must not trigger even though the
// method name matches. [MyMap#get] ignores its key argument and
// always returns [this.stored]; seeding [m.stored] with [source()]
// makes the intrafile call analyser carry taint through the opaque
// call and the sink fires. If the rewrite fired wrongly, [m.body]
// would project a clean cell via the shape layer (MyMap has no
// [body] field) and the finding would disappear.

