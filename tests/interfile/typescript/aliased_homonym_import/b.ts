export class A {
  m(x: string): void {
    // ruleid: aliased-homonym-import
    sink(x);
  }
}
