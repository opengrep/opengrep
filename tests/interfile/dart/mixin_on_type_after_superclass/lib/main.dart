import 'parts.dart';

String source() {
  return 'tainted';
}

class C extends A with M {
  void run() {
    f(source());
  }
}
