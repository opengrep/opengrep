// https://github.com/opengrep/opengrep/issues/119
fn f(path: String) {
    //ERROR: match
    let a = vec!["-c".into(), path];
    //ERROR: match
    let b = vec![foo(path), path.into()];
    let c = vec![path.to_string()];
    let d = vec![into(path)];
}
