// https://github.com/opengrep/opengrep/pull/855
fn f() {
    //ERROR: match
    let v = vec![&mut a, &mut b];
    //ERROR: match
    let v = vec![&mut a, b];
    let v = vec![&mut a];
}
