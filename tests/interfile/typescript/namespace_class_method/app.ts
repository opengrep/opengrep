import { Svc } from "./lib";

interface Req { query: { [k: string]: string } }

export function handle(req: Req) {
  const userInput = req.query.q;
  const h = new Svc.Handler();
  return h.run(userInput);
}
