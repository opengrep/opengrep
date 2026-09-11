// Regression test: `new` should be parseable as a property name in type
// declarations, not only as a construct signature keyword.
// See https://github.com/opengrep/opengrep/issues/768

type A = { new: string };
type B = { new?: string };
type C = { new: number };
type D = { old: string; new: number };

interface E {
  new: string;
}

class F {
  new: string = "";
}

// Construct signatures (the pre-existing, valid use of `new` in types)
// must still parse correctly.
type G = { new (): string };
type H = { new (x: string): void };
