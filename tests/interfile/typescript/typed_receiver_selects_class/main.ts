import { Store } from "./a";

export function go(s: Store) {
  s.run(source());
}
