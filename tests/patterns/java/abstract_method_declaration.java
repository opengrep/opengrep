// A declaration pattern matches the abstract and the interface method,
// and not the concrete one.
abstract class Shape {
  //ERROR: match
  abstract void draw(int x);

  //OK:
  void paint(int x) {
    draw(x);
  }
}

interface Drawable {
  //ERROR: match
  void render(int x);
}
