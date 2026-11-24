struct S(String, String);

fn tainted_tuple_field() {
    let s = S { 0: source(), 1: String::new() };
    // ruleid: taint
    sink(s.0);
}

fn clean_tuple_field() {
    let s = S { 0: String::new(), 1: source() };
    // ok: taint
    sink(s.0);
}
