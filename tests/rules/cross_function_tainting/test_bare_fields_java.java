public class Store {
  static String s = "";
  String g = "";

  static void writeStatic() {
    s = source();
  }

  static String readStatic() {
    return s;
  }

  void writeInstance() {
    g = source();
  }

  String readInstance() {
    return g;
  }

  static void staticField() {
    writeStatic();
    // ruleid: test-bare-fields-java
    sink(readStatic());
    // ruleid: test-bare-fields-java
    sink(s);
  }

  void instanceField() {
    writeInstance();
    // ruleid: test-bare-fields-java
    sink(readInstance());
    // ruleid: test-bare-fields-java
    sink(g);
  }

  void cleanField() {
    // ok: test-bare-fields-java
    sink(readInstance());
  }

  void shadowParam(String g) {
    g = source();
  }

  void shadowLocal() {
    String g = source();
    use(g);
  }

  void shadowed() {
    shadowParam("x");
    shadowLocal();
    // ok: test-bare-fields-java
    sink(this.g);
  }

  void bareWriteThisRead() {
    writeInstance();
    // ruleid: test-bare-fields-java
    sink(this.g);
  }

  static void onObjects() {
    Store a = new Store();
    Store b = new Store();
    a.writeInstance();
    // ruleid: test-bare-fields-java
    sink(a.g);
    // ok: test-bare-fields-java
    sink(b.g);
  }
}
