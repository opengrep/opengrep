module 0x1::test_value_copies {
    struct S has copy, drop { f: u64 }

    fun by_struct(s: S) { s.f = source(); }

    fun by_ref(s: &mut S) { s.f = source(); }

    fun caller() {
        let a = S { f: 0 };
        by_struct(a);
        // ok: test-value-copies-move-on-aptos
        sink(a.f);
        let c = S { f: 0 };
        by_ref(&mut c);
        // ruleid: test-value-copies-move-on-aptos
        sink(c.f);
    }
}
