export namespace A {
  export class Store {
    run(x: string): void {
      // ruleid: qualified-parameter-type
      sink(x);
    }
  }
}

export namespace B {
  export class Store {
    run(x: string): void {
      // ok: qualified-parameter-type
      sink(x);
    }
  }
}

declare function sink(x: string): void;
