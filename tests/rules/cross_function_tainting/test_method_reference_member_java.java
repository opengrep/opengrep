import java.util.function.Consumer;

class Other {
  static void handle(String s) {
    // ruleid: test-method-reference-member-java
    sink(s);
  }
}

class Main {
  static void handle(String s) {
    // ok: test-method-reference-member-java
    sink(s);
  }

  void own(String s) {
    // ruleid: test-method-reference-member-java
    sink(s);
  }

  void run(Consumer<String> c) {
    c.accept(source());
  }

  void go(String own) {
    String handle = "local";
    run(Other::handle);
    run(this::own);
  }
}
