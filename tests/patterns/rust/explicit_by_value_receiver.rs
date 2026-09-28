struct S { v: i32 }
impl S {
    fn e(self: &Self) -> i32 { 4 }
    fn f(self: &mut Self) -> i32 { 5 }
    // MATCH:
    fn g(self: Self) -> i32 { 6 }
}
