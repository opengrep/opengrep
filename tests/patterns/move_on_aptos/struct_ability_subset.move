module 0xcafe::struct_ability_subset {
    // ERROR: match
    struct KeyStore has key, store {
        a: u64,
    }

    // ERROR: match
    struct StoreKey has store, key {
        a: u64,
    }

    // ok: struct
    struct StoreOnly has store {
        a: u64,
    }

    // ERROR: match
    struct Postfix {
        a: u64,
    } has key;

    fun test_struct_ability_subset() {
    }
}
