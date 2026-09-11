export class Base {
  handle(x: string) {
    // ruleid: super-call-runs-parent
    sink(x);
  }
}
