// An abstract or interface method declares no body, so a pattern with a
// body does not match it; a declaration pattern does (see the sibling
// test). Dispatch merges the implementations into the declaration for
// taint, which does not give it a body.
abstract class Shape {
  //OK:
  abstract void draw(int x);

  //ERROR: match
  void paint(int x) {
    draw(x);
  }
}

interface Drawable {
  //OK:
  void render(int x);
}
