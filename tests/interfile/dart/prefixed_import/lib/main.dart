import 'dart:io';
import 'a.dart' as a;
import 'b.dart' as b;

String source() {
  return Platform.environment['SECRET'] ?? '';
}

void main() {
  var t = source();
  a.handle(t);
}
