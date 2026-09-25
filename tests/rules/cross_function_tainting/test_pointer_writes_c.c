void set(char **p) { *p = source(); }

void caller() {
  char *s = "";
  set(&s);
  // ruleid: test-pointer-writes-c
  sink(s);
}
