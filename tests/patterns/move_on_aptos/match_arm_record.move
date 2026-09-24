module 0xcafe::match_arm_record {
    enum E has copy, drop {
        V { a: u64, b: u64 },
        W { a: u64 },
    }

    fun test_match_arm_record(e: E, f: E) {
        // ERROR: match
        match (e) {
            E::V { b, a: x } => x,
            E::W { a } => a,
        };
        match (f) {
            E::W { a } => a,
            _ => 0,
        };
    }
}
