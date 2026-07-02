// Comprehensive HOF test for Dart: custom and built-in higher-order functions
// All annotated sinks should detect taint flow from source() to sink()

// ===== Custom HOF functions =====

List customMap(List arr, Function callback) {
  var result = [];
  for (var item in arr) {
    result.add(callback(item));
  }
  return result;
}

void customForEach(List arr, Function callback) {
  for (var item in arr) {
    callback(item);
  }
}

void directCall(Function callback, value) {
  callback(value);
}

// ===== Custom HOF test cases =====

void testCustomMap() {
  var arr = [source()];
  customMap(arr, (x) {
    // ruleid: test-hof-taint
    sink(x);
    return x;
  });
}

void testCustomForEach() {
  var arr = [source()];
  customForEach(arr, (x) {
    // ruleid: test-hof-taint
    sink(x);
  });
}

void testDirectCall() {
  directCall((x) {
    // ruleid: test-hof-taint
    sink(x);
  }, source());
}

// ===== Built-in iterable methods =====

void testBuiltinMap() {
  var arr = [source()];
  arr.map((x) {
    // ruleid: test-hof-taint
    sink(x);
    return x;
  });
}

void testBuiltinForEach() {
  var arr = [source()];
  arr.forEach((x) {
    // ruleid: test-hof-taint
    sink(x);
  });
}

void testBuiltinWhere() {
  var arr = [source()];
  arr.where((x) {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}

void testBuiltinReduce() {
  var arr = [source()];
  arr.reduce((acc, element) {
    // ruleid: test-hof-taint
    sink(element);
    return acc;
  });
}

// ===== Tear-offs (function references as callbacks) =====

void handler(x) {
  // ruleid: test-hof-taint
  sink(x);
}

void testBuiltinTearoff() {
  var arr = [source()];
  arr.forEach(handler);
}

void testCustomTearoff() {
  customForEach([source()], handler);
}

// ===== Chained HOF result =====

void testMapChainResult() {
  var input = source();
  var mapped = [input].map((e) => e).toList();
  // ruleid: test-hof-taint
  sink(mapped);
}
