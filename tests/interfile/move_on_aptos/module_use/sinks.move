module 0x1::sinks {
    public fun sink(x: u64): u64 {
        x
    }

    public fun relay(x: u64): u64 {
        // ruleid: test-move_on_aptos-module-use
        sink(x)
    }

    public fun quiet(x: u64): u64 {
        // ok: test-move_on_aptos-module-use
        sink(x)
    }
}
