export class A {
  m(x: string): void {
    // Never reached: app.ts constructs b.ts's A, not this one.
    // ok: aliased-homonym-import
    sink(x);
  }
}
