module 0x1::test_mut_binding {
    public struct S has copy, drop { f: u64, v: vector<u64> }

    fun set(s: &mut S) { s.f = source(); }

    fun set_element(s: &mut S) { s.v[0] = source(); }

    fun reassigned() {
        let mut x = 0;
        x = source();
        // ruleid: test-mut-binding-move-on-sui
        sink(x);
    }

    fun through_reference() {
        let mut d = S { f: 0, v: vector[] };
        set(&mut d);
        // ruleid: test-mut-binding-move-on-sui
        sink(d.f);
        let mut e = S { f: 0, v: vector[] };
        set_element(&mut e);
        // ruleid: test-mut-binding-move-on-sui
        sink(e.v);
        // ok: test-mut-binding-move-on-sui
        sink(e.f);
    }

    fun field_operators(mut s: S) {
        s.f = source();
        // ruleid: test-mut-binding-move-on-sui
        sink(s.f + 1);
        // ruleid: test-mut-binding-move-on-sui
        sink(s.f as u128);
    }
}
