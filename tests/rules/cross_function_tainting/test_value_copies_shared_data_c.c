struct Inner { char *x; };
struct S { char **p; struct Inner in; };

void setThroughPointer(struct S s, char *v) { s.p[0] = v; }

void setInner(struct S s, char *v) { s.in.x = v; }

void caller(void) {
  struct S a;
  setThroughPointer(a, source());
  // ruleid: test-value-copies-shared-data-c
  sink(a.p);
  struct S b;
  setInner(b, source());
  // ok: test-value-copies-shared-data-c
  sink(b.in.x);
}
