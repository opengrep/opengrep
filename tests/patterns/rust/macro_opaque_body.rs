// https://github.com/opengrep/opengrep/pull/855
// A macro body which is not an expression list stays opaque, calls in it
// are not parsed.
fn f(user: String) {
    //ERROR: match
    vec![sink(user)];
    foo! { fn g() -> T { sink(user) } };
    tokio::select! { v = rx => { sink(user) } };
    foo!(unsafe { sink(user) });
    foo!(a b sink(user));
    foo!(a b format!("{}", sink(user)));
    foo!(format!("{}", sink(user)) b);
}
