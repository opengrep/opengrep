// https://github.com/opengrep/opengrep/pull/855
fn f() {
    //ERROR: match
    foo!(mut x);
    //ERROR: match
    foo!(x);
    foo!(a b);
    foo!(mut a b);
}
