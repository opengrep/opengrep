// https://github.com/opengrep/opengrep/pull/855
fn f(t: String) {
    let mut w = String::new();
    //ERROR: match
    writeln!(&mut w, "{}", t);
    //ERROR: match
    writeln!(&mut w, "{}", t.trim());
    writeln!(w, "{}", t);
}
