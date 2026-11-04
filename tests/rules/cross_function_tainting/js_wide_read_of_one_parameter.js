// A function that reads nine fields of a parameter, within the bound on the
// paths of one input.
function nine(o) {
  return o.a + o.b + o.c + o.d + o.e + o.f + o.g + o.h + o.i;
}

// A function that reads seventeen fields below one field of a parameter, past
// the bound: the paths collapse to the field that holds them.
function nested(o) {
  return o.x.a + o.x.b + o.x.c + o.x.d + o.x.e + o.x.f + o.x.g + o.x.h + o.x.i + o.x.j + o.x.k + o.x.l + o.x.m + o.x.n + o.x.o + o.x.p + o.x.q;
}

function main() {
  // ruleid: wide-read-of-one-parameter
  sink(nine({ a: "c", b: "c", c: "c", d: "c", e: source(), f: "c", g: "c", h: "c", i: "c" }));
  // ok: wide-read-of-one-parameter
  sink(nine({ a: "c", b: "c", c: "c", d: "c", e: "c", f: "c", g: "c", h: "c", i: "c", z: source() }));
  // ruleid: wide-read-of-one-parameter
  sink(nested({ x: { a: "c", b: "c", c: "c", d: "c", e: source(), f: "c", g: "c", h: "c", i: "c", j: "c", k: "c", l: "c", m: "c", n: "c", o: "c", p: "c", q: "c" } }));
  // ok: wide-read-of-one-parameter
  sink(nested({ x: { a: "c", b: "c", c: "c", d: "c", e: "c", f: "c", g: "c", h: "c", i: "c", j: "c", k: "c", l: "c", m: "c", n: "c", o: "c", p: "c", q: "c" }, z: source() }));
}
