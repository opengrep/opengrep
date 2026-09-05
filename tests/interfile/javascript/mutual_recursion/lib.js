function cond() {
  return false;
}

function p(x) {
  return q(x);
}

function q(x) {
  return r(source());
}

function r(x) {
  if (cond()) {
    return p(x);
  }
  return x;
}
