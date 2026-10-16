// https://github.com/opengrep/opengrep/pull/855
// A call in these macros' arguments is not run.
fn f(user: String) {
    //ERROR: match
    println!("{}", sink(user));
    stringify!(sink(user));
    cfg!(sink(user));
    matches!(user, sink(user));
    assert_matches!(user, sink(user));
    debug_assert_matches!(user, sink(user));
    std::matches!(user, sink(user));
    assert!(matches!(user, sink(user)));
}
