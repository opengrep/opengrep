part of 'main.dart';

void sink(String x) {
  print(x);
}

class Base {
  void _leak(String x) {
    // ruleid: private-member-of-part-file-inherited
    sink(x);
  }
}
