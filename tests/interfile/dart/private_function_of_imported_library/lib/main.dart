import 'dart:io';
import 'helpers.dart';

String source() {
  return Platform.environment['SECRET'] ?? '';
}

void main() {
  handle(source());
  _handle(source());
}
