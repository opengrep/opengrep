package p;

public class Main {
  void staticField() {
    Store.writeStatic();
    // ruleid: bare-fields-across-files-java
    sink(Store.readStatic());
  }

  void instanceField() {
    Store a = new Store();
    Store b = new Store();
    a.writeInstance();
    // ruleid: bare-fields-across-files-java
    sink(a.readInstance());
    // ok: bare-fields-across-files-java
    sink(b.readInstance());
  }
}
