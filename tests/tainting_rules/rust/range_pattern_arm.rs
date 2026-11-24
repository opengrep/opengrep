fn tainted_in_range_arm() {
    let s = source();
    let x = 3;
    match x {
        // ruleid: taint
        1..=5 => sink(s),
        _ => {}
    }
}

fn clean_in_range_arm() {
    let s = String::new();
    let x = 3;
    match x {
        // ok: taint
        1..=5 => sink(s),
        _ => {}
    }
}

fn out_of_range_arm() {
    let s = source();
    let x = 100;
    match x {
        // ok: taint
        1..=5 => sink(s),
        _ => {}
    }
}

fn exclusive_upper_bound() {
    let s = source();
    let x = 5;
    match x {
        // ok: taint
        1..5 => sink(s),
        _ => {}
    }
}

fn left_unbounded_out_of_range() {
    let s = source();
    let x = 100;
    match x {
        // ok: taint
        ..=5 => sink(s),
        _ => {}
    }
}

fn right_unbounded_in_range() {
    let s = source();
    let x = 30;
    match x {
        // ruleid: taint
        10.. => sink(s),
        _ => {}
    }
}

fn right_unbounded_out_of_range() {
    let s = source();
    let x = 3;
    match x {
        // ok: taint
        10.. => sink(s),
        _ => {}
    }
}

fn char_range_in() {
    let s = source();
    let c = 'm';
    match c {
        // ruleid: taint
        'a'..='z' => sink(s),
        _ => {}
    }
}

fn char_range_out() {
    let s = source();
    let c = 'A';
    match c {
        // ok: taint
        'a'..='z' => sink(s),
        _ => {}
    }
}

fn byte_range_out() {
    let s = source();
    let b = b'A';
    match b {
        // ok: taint
        b'a'..=b'z' => sink(s),
        _ => {}
    }
}

fn float_range_in() {
    let s = source();
    let f = 0.5;
    match f {
        // ruleid: taint
        0.0..=1.0 => sink(s),
        _ => {}
    }
}

fn float_range_out() {
    let s = source();
    let f = 2.5;
    match f {
        // ok: taint
        0.0..=1.0 => sink(s),
        _ => {}
    }
}
