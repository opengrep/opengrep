module 0x1::test_let_destructure {
    struct S has copy, drop { f: u64, g: u64 }
    struct Outer has copy, drop { inner: S, h: u64 }
    struct P(u64, u64) has copy, drop;

    enum E has copy, drop {
        V { a: u64, b: u64 },
    }

    fun by_record() {
        let s = S { f: source(), g: 0 };
        // ok: test-let-destructure-move-on-aptos
        sink(s.g);
        let S { f, g } = s;
        // ruleid: test-let-destructure-move-on-aptos
        sink(f);
        // ok: test-let-destructure-move-on-aptos
        sink(g);
    }

    fun by_nested_record() {
        let o = Outer { inner: S { f: source(), g: 0 }, h: 0 };
        let Outer { inner: S { f, g }, h } = o;
        // ruleid: test-let-destructure-move-on-aptos
        sink(f);
        // ok: test-let-destructure-move-on-aptos
        sink(g);
        // ok: test-let-destructure-move-on-aptos
        sink(h);
    }

    fun by_tuple() {
        let (x, y) = (source(), 0);
        // ruleid: test-let-destructure-move-on-aptos
        sink(x);
        // ok: test-let-destructure-move-on-aptos
        sink(y);
    }

    fun by_position() {
        let P(x, y) = P(source(), 0);
        // ruleid: test-let-destructure-move-on-aptos
        sink(x);
    }

    fun by_match_arm(e: E) {
        let e = E::V { a: source(), b: 0 };
        match (e) {
            E::V { a, b } => {
                // ruleid: test-let-destructure-move-on-aptos
                sink(a);
            },
        }
    }
}
