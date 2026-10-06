// Methods of an object literal inside a function are closures of that
// function: each has a signature of its own (two methods of one literal are
// told apart) and reads the variables it captures. The same inside a class
// method.
function setup(options) {
  const settings = options.datasources || [];
  install({
    getList() {
      return settings.map((s) => s.name);
    },
    getInstanceSettings(ref) {
      const all = settings.map((s) => s.name);
      // ruleid: lambda-sig-js-object-methods
      return all.find((x) => x === ref) || all[0];
    },
  });
}

class Svc {
  setup(options) {
    const settings = options.datasources || [];
    install({
      getList() {
        return settings.map((s) => s.name);
      },
      getInstanceSettings(ref) {
        const all = settings.map((s) => s.name);
        // ruleid: lambda-sig-js-object-methods
        return all.find((x) => x === ref) || all[0];
      },
    });
  }
}

function test() {
  const data = JSON.parse(JSON.stringify(fixture));
  setup({ datasources: data });
  new Svc().setup({ datasources: data });
}
