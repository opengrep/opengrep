trait A

//ERROR:
class C extends A, Serializable

//OK:
class D extends A
