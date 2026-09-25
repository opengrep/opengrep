module 0xcafe::let_mut {
    fun test_let_mut() {
        // ERROR: match
        let mut x = 1;
        let y = 2;
        x = y;
    }
}
