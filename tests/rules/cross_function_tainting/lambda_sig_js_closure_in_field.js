// A closure held by a field is called through the field.
function run(options) {
  const handlers = {
    emit: (x) => {
      // ruleid: lambda-sig-js-closure-in-field
      sink(x);
    },
  };
  handlers.emit(options.value);
}
function go() {
  run({ value: source() });
}
