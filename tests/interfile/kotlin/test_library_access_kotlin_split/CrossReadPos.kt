fun CrossReadPos() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", source())
    // ruleid: test-library-access-taint
    sink(readBody(m))
}
