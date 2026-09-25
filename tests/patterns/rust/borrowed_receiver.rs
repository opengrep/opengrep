struct S { v: i32 }
impl S {
    // MATCH:
    fn a(&self) -> i32 { 0 }
    // MATCH:
    fn b(&mut self) -> i32 { 1 }
    fn c(self) -> i32 { 2 }
    fn d(mut self) -> i32 { 3 }
    // MATCH:
    fn e(self: &Self) -> i32 { 4 }
}
