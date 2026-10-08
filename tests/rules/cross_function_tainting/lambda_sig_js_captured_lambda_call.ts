// A callback calls a function it reaches through a captured variable: a
// variable holding a lambda, whose result it reads, or an object holding a
// function, whose sink its arguments reach.
describe('exporter', () => {
  const setup = async () => {
    const copy = JSON.parse(JSON.stringify(schema));
    return { dashboard: copy };
  };
  it('reads what setup returns', async () => {
    const { dashboard } = await setup();
    // ruleid: lambda-sig-js-captured-lambda-call
    const variable = dashboard.variables[0];
  });
});

(function () {
  function readKey(obj, key) {
    // ruleid: lambda-sig-js-captured-lambda-call
    return obj[key];
  }
  var Utils = { readKey: readKey };
  var run = function (obj) {
    return Utils.readKey(obj, JSON.stringify(obj));
  };
  run({});
})();
