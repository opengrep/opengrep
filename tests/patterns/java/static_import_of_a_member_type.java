import static a.B.Inner;

class T {
  void f(Inner x, Other y) {
    //ERROR: match
    sink(x);
    sink(y);
  }
}
