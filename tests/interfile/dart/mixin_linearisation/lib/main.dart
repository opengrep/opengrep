import 'parts.dart';

String source() {
  return 'tainted';
}

class C extends A with M1, M2 {
  void run() {
    f(source());
  }
}
