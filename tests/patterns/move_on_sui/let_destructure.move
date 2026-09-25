module 0xcafe::let_destructure {
    public struct S has copy, drop { f: u64, g: u64 }
    public struct T has copy, drop { f: u64 }

    fun test_let_destructure(s: S, t: T) {
        // ERROR: match
        let S { f: a, g } = s;
        // ERROR: match
        let S { g, f: b } = s;
        let T { f: c } = t;
        // ERROR: match
        let S { g: d, f: _ } = s;
    }
}
