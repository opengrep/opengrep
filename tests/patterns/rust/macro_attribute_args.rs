// https://github.com/opengrep/opengrep/pull/855
// An attribute's arguments are not run.
#[instrument(skip(password))]
fn login(password: String) {
    //ERROR: match
    skip(password);
}
