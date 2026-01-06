// Test: Kotlin collection models for taint propagation

fun testListAdd(tainted: String) {
    val list = mutableListOf<String>()
    list.add(tainted)
    // ruleid: kotlin-collection-taint
    sink(list)
}

fun testMapPut(tainted: String) {
    val map = mutableMapOf<String, String>()
    map.put("key", tainted)
    val value = map.get("key")
    // ruleid: kotlin-collection-taint
    sink(value)
}

fun testListFirst(tainted: String) {
    val list = mutableListOf(tainted)
    val value = list.first()
    // ruleid: kotlin-collection-taint
    sink(value)
}

fun testListLast(tainted: String) {
    val list = mutableListOf(tainted)
    val value = list.last()
    // ruleid: kotlin-collection-taint
    sink(value)
}

fun sink(data: Any?) {
    println(data)
}
