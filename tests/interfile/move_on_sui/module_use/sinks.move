module pkg::sinks {
    public fun sink(x: u64): u64 {
        x
    }

    public fun relay(x: u64): u64 {
        // ruleid: test-move_on_sui-module-use
        sink(x)
    }

    public fun quiet(x: u64): u64 {
        // ok: test-move_on_sui-module-use
        sink(x)
    }
}
