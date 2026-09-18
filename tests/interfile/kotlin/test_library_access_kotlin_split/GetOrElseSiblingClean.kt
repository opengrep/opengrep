fun GetOrElseSiblingClean() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("user", source())
    // ok: test-library-access-taint
    sink(m.getOrElse("body") { "fallback" })
}
