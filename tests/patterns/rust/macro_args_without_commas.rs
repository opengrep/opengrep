// https://github.com/opengrep/opengrep/issues/119
fn f() {
    //ERROR: match
    foo!(a);
    foo!(a b);
    foo!(x y z);
}
