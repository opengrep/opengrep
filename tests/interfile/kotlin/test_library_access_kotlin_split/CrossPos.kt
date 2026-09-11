fun CrossPos() {
    val m: HashMap<String, String> = HashMap<String, String>()
    writeBody(m, source())
    // ruleid: test-library-access-taint
    sink(m.get("body"))
}
