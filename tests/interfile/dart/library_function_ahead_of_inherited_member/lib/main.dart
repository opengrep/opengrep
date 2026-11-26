import 'base.dart';

String source() {
  return 'tainted';
}

void leak(String x) {
  // ruleid: library-function-ahead-of-inherited-member
  sink(x);
}

class Main extends Base {
  void run() {
    leak(source());
  }
}
