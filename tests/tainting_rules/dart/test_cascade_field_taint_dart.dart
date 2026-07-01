// A Dart cascade desugars to a hidden receiver variable plus the section
// operations, so a tainted field assignment inside a cascade
// (obj..x = source()) taints obj.x, while other fields stay clean.
void main(obj, input) {
  obj..y = 1..x = source(input)..z = 2;
  // ruleid: cascade-field-taint
  sink(obj.x);

  obj..w = 1;
  // ok: cascade-field-taint
  sink(obj.w);
}
