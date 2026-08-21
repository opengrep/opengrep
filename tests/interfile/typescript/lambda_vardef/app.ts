import { handler } from "./handlers";

interface Req { query: { [k: string]: string } }

export function handle(req: Req) {
  // SOURCE: user-controlled query param.
  const userInput = req.query.q;
  // Cross-file call into the lambda VarDef in handlers.ts.
  return handler(userInput);
}
