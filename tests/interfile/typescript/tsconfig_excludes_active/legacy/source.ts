import { helper } from "../helper";

interface Req { query: { [k: string]: string } }

// SOURCE-bearing file.  Lives under legacy/, which tsconfig.build.json
// excludes.  If projidx prefers tsconfig.build.json over tsconfig.json
// (as § 4 TS claims), this file is NOT in the call graph, no edge is
// built into helper.ts, and the cross-file taint chain is broken.
export function legacyHandle(req: Req) {
  const userInput = req.query.q;
  return helper(userInput);
}
