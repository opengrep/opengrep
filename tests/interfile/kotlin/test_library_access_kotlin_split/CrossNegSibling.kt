fun CrossNegSibling() {
    val m: HashMap<String, String> = HashMap<String, String>()
    m.put("body", "safe")
    writeUser(m, source())
    // ok: test-library-access-taint
    sink(m.get("body"))
}
