// Built-in collection and StringBuffer models: mutators taint the receiver,
// accessors propagate the receiver's taint to their return value.

void testListAdd() {
  var list = <String>[];
  list.add(source());
  // ruleid: test-collection-models-dart
  sink(list.join(","));
}

void testListInsert() {
  var list = <String>[];
  list.insert(0, source());
  // ruleid: test-collection-models-dart
  sink(list.removeAt(0));
}

void testListAddAll() {
  var list = <String>[];
  list.addAll([source()]);
  // ruleid: test-collection-models-dart
  sink(list.elementAt(0));
}

void testListFirst() {
  var list = <String>[];
  list.add(source());
  // ruleid: test-collection-models-dart
  sink(list.first);
}

void testStringBuffer() {
  var buf = StringBuffer();
  buf.writeln(source());
  // ruleid: test-collection-models-dart
  sink(buf.toString());
}

void testCleanCollection() {
  var list = <String>[];
  list.add("clean");
  sink(list.join(","));
}
