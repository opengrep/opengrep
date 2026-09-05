function handleRequestSafe({user: {profile: {body}}}) {
  // ok: test-destructure-depth3-js
  sink(body);
}
