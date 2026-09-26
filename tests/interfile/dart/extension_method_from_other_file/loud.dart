import 'box.dart';

extension Loud on Box {
  void handle(String x) {
    // ruleid: extension-method-from-other-file
    sink(x);
  }
}
