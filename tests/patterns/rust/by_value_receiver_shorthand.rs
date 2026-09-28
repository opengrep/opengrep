struct S { v: i32 }
impl S {
    fn a(&self) -> i32 { 0 }
    fn b(&mut self) -> i32 { 1 }
    // MATCH:
    fn c(self) -> i32 { 2 }
    // MATCH:
    fn d(mut self) -> i32 { 3 }
}
