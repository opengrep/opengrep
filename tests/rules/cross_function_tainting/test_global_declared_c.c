int G;
int C;

void write_tainted() {
  G = source();
}

void write_clean() {
  C = 0;
}

void read_after_write() {
  write_tainted();
  // ruleid: test-global-declared-c
  sink(G);
}

void read_clean() {
  write_clean();
  // ok: test-global-declared-c
  sink(C);
}

void read_shadowed() {
  int G = 0;
  write_tainted();
  // ok: test-global-declared-c
  sink(G);
}
