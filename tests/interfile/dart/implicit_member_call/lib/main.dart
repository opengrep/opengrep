import 'base.dart';

String source() {
  return 'tainted';
}

class Main extends Base {
  void run() {
    leak(source());
    keep('constant');
  }
}
