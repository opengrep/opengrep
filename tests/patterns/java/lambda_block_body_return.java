class T {
  void test() {
    // A block-bodied lambda returns only what it says; its last
    // statement is not a return.
    Runnable f = () -> {
      //OK:
      g(t);
    };
    Supplier<Object> k = () -> {
      //ERROR: match
      return g(t);
    };
  }
}
