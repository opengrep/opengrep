import 'dart:io';
import 'store.dart';

String source() {
  return Platform.environment['SECRET'] ?? '';
}

void main() {
  var tainted = source();
  var s = Store(data: tainted);
  s.send();
}
