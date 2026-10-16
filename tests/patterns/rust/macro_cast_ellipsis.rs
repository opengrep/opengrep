// https://github.com/opengrep/opengrep/pull/855
fn f(b: u8) {
    //ERROR: match
    let s = format!("{:x}", b as u32);
    //ERROR: match
    let t = format!("{:x} {}", b as u32, b as u64);
    let u = print!("{:x}", b as u32);
}
