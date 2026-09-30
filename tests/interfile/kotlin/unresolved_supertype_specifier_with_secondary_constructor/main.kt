package app

import vendor.Ext
import vendor.Ext2

fun sink(x: String) {}

fun source(): String = System.getenv("SECRET")

class D : Ext {
    constructor(n: Int) : super()

    fun handle(x: String) {
        // ruleid: unresolved-supertype-specifier-with-secondary-constructor
        sink(x)
    }
}

class E : Ext, Ext2 {
    constructor(n: Int) : super()

    fun process(x: String) {
        // ruleid: unresolved-supertype-specifier-with-secondary-constructor
        sink(x)
    }
}

fun run() {
    D(1).handle(source())
    E(1).process(source())
}
