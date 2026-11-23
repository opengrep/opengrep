const LIMIT: usize = 8;
// MATCH:
struct S<const N: usize = LIMIT>;
struct U<const N: usize = OTHER>;
