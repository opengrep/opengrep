import { Runner, Cleaner } from "./runner";

function run() {
  const r = new Runner();
  r.handler(taint());
  const c = new Cleaner();
  c.handler(taint());
}
