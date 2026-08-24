subscripted = "abc";
subscripted[0] = "x";

fielded = "abc";
fielded.len = 3;

nested = "abc";
nested.a[0].b = 1;

plain = "abc";

// A store into a temporary literal does not write the variables inside it.
temp = "abc";
[temp][0] = "x";
[temp].length = 0;

function use() {
  // A subscript store counts as a write to `subscripted`.
  // ok: cp-container-store-write
  f(subscripted);
  // A field store counts as a write to `fielded`.
  // ok: cp-container-store-write
  f(fielded);
  // Mixed field/subscript chains are peeled down to the base variable.
  // ok: cp-container-store-write
  f(nested);
  // ruleid: cp-container-store-write
  f(plain);
  // ruleid: cp-container-store-write
  f(temp);
}
