fun GetOrElseLambdaTainted() {
    val m: HashMap<String, String> = HashMap<String, String>()
    // ruleid: test-library-access-taint
    sink(m.getOrElse("body") { source() })
}
