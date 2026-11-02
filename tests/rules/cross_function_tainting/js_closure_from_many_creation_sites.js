function mk(v) { return () => v; }

function tainted(c) {
  let g;
  if (c == 0) g = mk(source());
  else if (c == 1) g = mk("s1");
  else if (c == 2) g = mk("s2");
  else if (c == 3) g = mk("s3");
  else if (c == 4) g = mk("s4");
  else if (c == 5) g = mk("s5");
  else if (c == 6) g = mk("s6");
  else if (c == 7) g = mk("s7");
  else if (c == 8) g = mk("s8");
  else g = mk("s9");
  return g;
}

function clean(c) {
  let g;
  if (c == 0) g = mk("s0");
  else if (c == 1) g = mk("s1");
  else if (c == 2) g = mk("s2");
  else if (c == 3) g = mk("s3");
  else if (c == 4) g = mk("s4");
  else if (c == 5) g = mk("s5");
  else if (c == 6) g = mk("s6");
  else if (c == 7) g = mk("s7");
  else if (c == 8) g = mk("s8");
  else g = mk("s9");
  return g;
}

function top(c) {
  // ruleid: closure-from-many-creation-sites
  sink(tainted(c)());
  // ok: closure-from-many-creation-sites
  sink(clean(c)());
}
