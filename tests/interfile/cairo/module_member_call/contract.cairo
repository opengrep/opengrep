mod contract {
    fn source() -> felt252 {
        1
    }

    fn sink(x: felt252) -> felt252 {
        x
    }

    fn leak(x: felt252) -> felt252 {
        // ruleid: test-cairo-module-member
        sink(x)
    }

    fn keep(x: felt252) -> felt252 {
        // ok: test-cairo-module-member
        sink(x)
    }

    fn run() -> felt252 {
        let tainted = leak(source());
        let clean = keep(2);
        tainted + clean
    }
}
