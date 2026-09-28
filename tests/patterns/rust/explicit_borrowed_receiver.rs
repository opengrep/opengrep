struct S { v: i32 }
impl S {
    // MATCH:
    fn e(self: &Self) -> i32 { 4 }
    // MATCH:
    fn f(self: &mut Self) -> i32 { 5 }
    fn g(self: Self) -> i32 { 6 }
}
