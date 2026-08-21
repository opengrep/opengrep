import { Runner } from "./runner";

function run() {
  const r = new Runner();
  r.handler(taint());
}
