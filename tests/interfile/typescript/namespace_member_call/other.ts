export namespace Svc {
  export function run(x: string) {
    // ok: namespace-member-call
    sink(x);
  }
}
