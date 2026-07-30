fn f() {
    //ERROR: match
    let Config { timeout, .. } = load();
}
