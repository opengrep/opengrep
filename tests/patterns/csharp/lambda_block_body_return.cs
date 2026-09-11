class T {
  void Test() {
    // A block-bodied lambda returns only what it says; its last
    // statement is not a return.
    Action f = () => {
      // OK:
      G(t);
    };
    Func<object> k = () => {
      // ERROR:
      return G(t);
    };
  }
}
