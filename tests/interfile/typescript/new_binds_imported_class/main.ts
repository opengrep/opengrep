import { Store } from "./a";

export function go() {
  const s = new Store();
  s.run(source());
}
