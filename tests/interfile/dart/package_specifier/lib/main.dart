import 'dart:io';
import 'package:package_specifier/a.dart' as a;
import 'package:package_specifier/b.dart' as b;

String source() {
  return Platform.environment['SECRET'] ?? '';
}

void main() {
  var t = source();
  a.handle(t);
}
