// https://github.com/opengrep/opengrep/pull/855
// *p as u64 casts the dereference, and p as u64 does not dereference.
fn f(p: &u8) {
    //ERROR: match
    println!("{}", *p as u64);
    println!("{}", p as u64);
    println!("{}", &p as u64);
}
