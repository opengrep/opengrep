package app

import vendor.Ext

fun sink(x: String) {}

fun source(): String = System.getenv("SECRET")

open class Base {
    open fun handle(x: String) {
        // ruleid: unresolved-supertype-specifier-with-primary-constructor
        sink(x)
    }
}

class A() : Ext

class B() : Base(), Ext

fun run() {
    A().accept(source())
    B().handle(source())
}
