function handler(data) {
  // ruleid: cjs-default-named-and-bare
  sink(data);
}

function bareOnly(data) {
  // ok: cjs-default-named-and-bare
  sink(data);
}

exports = bareOnly;
module.exports = handler;
