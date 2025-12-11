public class C(string x = "") {  }

public class D {
  public void foo() {
    val s = source();
    val c = new C();
    c.x = s;
    // ruleid: taint
    sink(c.x);
  }
}
