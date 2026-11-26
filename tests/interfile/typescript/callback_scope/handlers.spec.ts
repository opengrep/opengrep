import { sink, source } from "./lib";

describe("tainted", () => {
  const handle = (x: string) => {
    // ruleid: test-typescript-callback-scope
    return sink(x);
  };
  it("passes tainted data", () => {
    handle(source());
  });
});

describe("constant", () => {
  const handle = (x: string) => {
    // ok: test-typescript-callback-scope
    return sink(x);
  };
  it("passes a constant", () => {
    handle("constant");
  });
});
