class Base {}

class Derived extends Base {}

class DeclaredTypeSurvivesTaint {
    void f() {
        Base b = new Derived();
        // ruleid: declared_type_survives_taint
        foo(b);
    }

    void foo(Base x) {}

    void g() {
        // ruleid: declared_type_survives_taint_sink
        sink(source());
    }
}
