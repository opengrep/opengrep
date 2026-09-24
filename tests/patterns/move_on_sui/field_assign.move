module 0xcafe::field_assign {
    public struct S has copy, drop { f: u64, g: u64 }

    fun test_field_assign(s: &mut S) {
        // ERROR: match
        s.f = 1;
        s.g = 1;
        let f = 2;
    }
}
