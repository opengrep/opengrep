import { run } from "./a";

export function go() {
  const xs = [source()];
  xs.map(run);
}
