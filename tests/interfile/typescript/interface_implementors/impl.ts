import { Handler } from "./handler";

export class Impl implements Handler {
  handle(x: string): void {
    // ruleid: interface_implementors
    sink(x);
  }
}

export class Unrelated {
  handle(x: string): void {
    // ok: interface_implementors
    sink(x);
  }
}
