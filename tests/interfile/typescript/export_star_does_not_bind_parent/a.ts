export class Base {
  f(x: string): void {
    // ok: export-star-does-not-bind-parent
    sink(x);
  }
}
