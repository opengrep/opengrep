class A { void run() {} }

class B { void run() {} }

class MethodUnificationSurvivesTaint {
    void go(A a, B b) {
        // ruleid: method_unification_survives_taint
        a.run();
        b.run();
        Object x = source();
        // ruleid: method_unification_survives_taint_sink
        sink(x);
    }
}
