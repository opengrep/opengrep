interface I {
    fun handle(x: String)
}

class B : I {
    override fun handle(x: String) {}
}

//ERROR:
class D(b: B) : I by b

class E(b: B) : I {
    override fun handle(x: String) {}
}
