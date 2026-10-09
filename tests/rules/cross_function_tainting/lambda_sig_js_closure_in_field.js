// A closure held by a field is called through the field.
function run(options) {
  const handlers = {
    emit: (x) => {
      // ruleid: lambda-sig-js-closure-in-field
      sink(x);
    },
    drop: (y) => {
      // ok: lambda-sig-js-closure-in-field
      sink(y);
    },
  };
  handlers.emit(options.value);
  handlers.drop("a");
}
function go() {
  run({ value: source() });
}
