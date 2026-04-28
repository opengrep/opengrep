fun HandlerPutPos() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", source())
    m.put("user", "safe")
    // ruleid: taint
    sink(m.get("body"))
}

fun HandlerPutNeg() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", "safe")
    m.put("user", source())
    // ok: taint
    sink(m.get("body"))
}

fun HandlerSetPos() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.set("body", source())
    // ruleid: taint
    sink(m.getValue("body"))
}

fun HandlerGetOrDefaultPos() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", source())
    // ruleid: taint
    sink(m.getOrDefault("body", "fallback"))
}

fun HandlerGetOrDefaultTaintedDefault() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", "safe")
    // ruleid: taint
    sink(m.getOrDefault("body", source()))
}

fun HandlerPutIfAbsentPos() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.putIfAbsent("body", source())
    // ruleid: taint
    sink(m.get("body"))
}

fun HandlerRemoveClearsCell() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", source())
    m.remove("body")
    // ok: taint
    sink(m.get("body"))
}

fun HandlerRemoveReturnsPrior() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", source())
    // ruleid: taint
    sink(m.remove("body"))
}
