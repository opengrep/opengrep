// A callback that writes a captured variable, and one that reads it through a
// callback a built-in calls.
describe('x', () => {
  let prepared;
  let plain;
  beforeEach(() => {
    prepared = JSON.parse(JSON.stringify(mock));
    plain = { a: 1 };
  });
  it('y', () => {
    // ruleid: lambda-sig-js-captured-write
    const lines = [...prepared.diff_files].flatMap((file) => [...file[KEY]]);
  });
  it('z', () => {
    // ruleid: lambda-sig-js-captured-write
    expect(prepared[0].renderIt).toBeTruthy();
  });
  it('w', () => {
    // ok: lambda-sig-js-captured-write
    expect(plain[0]).toBeTruthy();
  });
});
