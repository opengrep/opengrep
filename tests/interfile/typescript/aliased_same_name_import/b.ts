export class A {
  m(x: string): void {
    // ruleid: aliased-same-name-import
    sink(x);
  }
}
