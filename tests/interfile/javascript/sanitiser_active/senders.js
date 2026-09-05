function sendClean(value) {
  const safe = sanitize(value);
  // ok: sanitiser-active-js
  sink(safe);
}

function sendDirty(value) {
  // ruleid: sanitiser-active-js
  sink(value);
}
