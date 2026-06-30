import java.util.function.Supplier;
class C {
    Supplier<X> f() {
        // method reference with an explicit type argument
        // MATCH:
        return Foo::<String>bar;
    }
}
