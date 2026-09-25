struct S { char *x; };
typedef struct { char *y; } T;
typedef struct S *SP;

void byValue(struct S s) { s.x = source(); }
void byTypedef(T t) { t.y = source(); }
void byPointer(struct S *s) { s->x = source(); }
void byTypedefPointer(SP p) { p->x = source(); }

void caller() {
  struct S a;
  byValue(a);
  // ok: test-value-copies-c
  sink(a.x);
  T b;
  byTypedef(b);
  // ok: test-value-copies-c
  sink(b.y);
  struct S c;
  byPointer(&c);
  // ruleid: test-value-copies-c
  sink(c.x);
  struct S d;
  byTypedefPointer(&d);
  // ruleid: test-value-copies-c
  sink(d.x);
}
