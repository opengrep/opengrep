// Comprehensive HOF test: Custom functions and all built-in array methods
// All of these should detect taint flow from source() to sink()

// ===== Custom HOF Functions =====

function customMap(arr, callback) {
  const result = [];
  for (let i = 0; i < arr.length; i++) {
    result.push(callback(arr[i]));
  }
  return result;
}

function customForEach(arr, callback) {
  for (let i = 0; i < arr.length; i++) {
    callback(arr[i]);
  }
}

function directCall(callback) {
  callback(source());
}

// ===== Test Cases =====

function test_custom_map() {
  const arr = [source()];
  customMap(arr, (x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}

function test_custom_foreach() {
  const arr = [source()];
  customForEach(arr, (x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}

function test_direct_call() {
  directCall((x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}

// ===== Built-in Array Methods =====

function test_builtin_map() {
  const arr = [source()];
  arr.map((x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}

function test_builtin_flatMap() {
  const arr = [source()];
  arr.flatMap((x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}

function test_builtin_filter() {
  const arr = [source()];
  arr.filter((x) => {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}

function test_builtin_forEach() {
  const arr = [source()];
  arr.forEach((x) => {
    // ruleid: test-hof-taint
    sink(x);
  });
}

function test_builtin_find() {
  const arr = [source()];
  arr.find((x) => {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}

function test_builtin_findIndex() {
  const arr = [source()];
  arr.findIndex((x) => {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}

function test_builtin_some() {
  const arr = [source()];
  arr.some((x) => {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}

function test_builtin_every() {
  const arr = [source()];
  arr.every((x) => {
    // ruleid: test-hof-taint
    sink(x);
    return true;
  });
}

function test_builtin_reduce() {
  const arr = [source()];
  arr.reduce((acc, x) => {
    // ruleid: test-hof-taint
    sink(x);
    return acc;
  }, []);
}

function test_builtin_reduceRight() {
  const arr = [source()];
  arr.reduceRight((acc, x) => {
    // ruleid: test-hof-taint
    sink(x);
    return acc;
  }, []);
}

// ===== Complex Example from Original Request =====

function getHistory(name, owner) {
  const result = source();
  return result;
}

async function test_original_example() {
  const history = await getHistory("name", "owner");
  const items = history.flatMap((node) => {
    const changes = node.associatedPullRequests.nodes;
    // ruleid: test-hof-taint
    return sink(changes);
  });
}
