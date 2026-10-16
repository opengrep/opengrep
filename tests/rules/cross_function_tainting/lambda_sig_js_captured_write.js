// A callback that writes a captured variable, and one that reads it through a
// callback a built-in calls.
group(() => {
  let data;
  let plain;
  before(() => {
    data = source();
    plain = { a: 1 };
  });
  check(() => {
    // ruleid: lambda-sig-js-captured-write
    data.items.map((item) => sink(item));
  });
  check(() => {
    // ruleid: lambda-sig-js-captured-write
    sink(data);
  });
  check(() => {
    // ok: lambda-sig-js-captured-write
    sink(plain);
  });
});
