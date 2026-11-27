// https://github.com/opengrep/opengrep/issues/119
fn f(path: String) {
    //ERROR: match
    let a = vec![format!("{}", path)];
    //ERROR: match
    let b = vec!["-c".into(), format!("C:{}", path)];
    //ERROR: match
    Command::new("cmd.exe").args(vec!["-c".into(), format!("C:{}", path)]);
    let c = vec![path];
    let d = format!("{}", path);
}
