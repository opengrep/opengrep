fn cond() -> bool {
    false
}

pub fn p(x: i32) -> i32 {
    q(x)
}

fn q(x: i32) -> i32 {
    r(source())
}

fn r(x: i32) -> i32 {
    if cond() {
        return p(x);
    }
    x
}
