// Methods of an object literal, in a function or in a class method, are its
// closures: each has a signature of its own and reads the variables it captures.
function setup(options) {
  const items = options.items;
  const names = ["a"];
  install({
    first() {
      // ok: lambda-sig-js-object-methods
      sink(names);
    },
    at() {
      // ruleid: lambda-sig-js-object-methods
      sink(items);
    },
  });
}

class Box {
  setup(options) {
    const items = options.items;
    install({
      at() {
        // ruleid: lambda-sig-js-object-methods
        sink(items);
      },
    });
  }
}

function test() {
  const data = source();
  setup({ items: data });
  new Box().setup({ items: data });
}
