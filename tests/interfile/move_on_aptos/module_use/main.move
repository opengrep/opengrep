module 0x1::main {
    use 0x1::sinks;

    fun source(): u64 {
        1
    }

    public fun run(): u64 {
        let tainted = sinks::relay(source());
        let clean = sinks::quiet(2);
        tainted + clean
    }
}
