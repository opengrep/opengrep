export namespace Svc {
  export namespace Inner {
    export function run(x: string) {
      // ruleid: import-alias-binds-namespace
      sink(x);
    }
  }
}
