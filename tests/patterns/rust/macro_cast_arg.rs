// https://github.com/opengrep/opengrep/pull/855
fn f(x: u32, n: u32, a: A) {
    //ERROR: match
    println!("{} {}", x, n as u64);
    //ERROR: match
    println!("{} {}", x, n as u8 as u64);
    //ERROR: match
    println!("{} {}", x, a.b as f64);
    println!("{}", n as u64);
}
