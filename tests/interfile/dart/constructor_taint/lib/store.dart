void sink(String x) {
  print(x);
}

class Store {
  final String data;

  Store({required this.data});

  void send() {
    // ruleid: test-constructor-taint
    sink(this.data);
  }
}
