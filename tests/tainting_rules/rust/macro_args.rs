// https://github.com/opengrep/opengrep/issues/119
use std::process::Command;

fn nested_macro(path: String) {
    // ruleid: taint
    Command::new("cmd.exe").args(vec!["-c".into(), format!("C:{}", path)]);
}

fn method_call(path: String) {
    // ruleid: taint
    Command::new("sh").args(vec!["-c".to_string(), path.to_string()]);
}

fn through_variable(path: String) {
    let v = vec![format!("{}", path)];
    // ruleid: taint
    Command::new("sh").args(v);
}

fn clean(path: String) {
    // ok: taint
    Command::new("sh").args(vec!["-c".into(), format!("C:{}", "safe")]);
}
