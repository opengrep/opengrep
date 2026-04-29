function handleRequest({user: {profile: {body}}}) {
  // ruleid: test-destructure-depth3-js
  sink(body);
}

function pos() {
  handleRequest({user: {profile: {body: source(), other: "safe"}}});
}


function handleRequestSafe({user: {profile: {body}}}) {
  // ok: test-destructure-depth3-js
  sink(body);
}

function neg() {
  handleRequestSafe({user: {profile: {body: "safe", other: source()}}});
}
