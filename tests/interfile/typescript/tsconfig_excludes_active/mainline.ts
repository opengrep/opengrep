import { helper } from "./helper";

// Non-excluded caller of helper.  Passes a CONSTANT (not a source
// pattern match), so no taint is introduced here.  Its purpose is
// solely to keep helper.ts reachable in the (post-exclude) call
// graph so the test doesn't pass trivially because the rule pruned
// helper.ts out of every subgraph.
export function safeUse(): string {
  return helper("constant");
}
