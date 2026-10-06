// TODO: a callback calling a function it reaches through a captured variable
// (a variable holding a lambda, or an object holding a function) sees an
// unknown call: a source created inside [setup], or a sink reached inside
// [readKey], is lost.
describe('exporter', () => {
  const setup = async () => {
    const copy = JSON.parse(JSON.stringify(schema));
    return { dashboard: copy };
  };
  it('reads what setup returns', async () => {
    const { dashboard } = await setup();
    // todoruleid: lambda-sig-js-captured-lambda-call
    const variable = dashboard.variables[0];
  });
});

(function () {
  function readKey(obj, key) {
    // todoruleid: lambda-sig-js-captured-lambda-call
    return obj[key];
  }
  var Utils = { readKey: readKey };
  var run = function (obj) {
    return Utils.readKey(obj, JSON.stringify(obj));
  };
  run({});
})();
