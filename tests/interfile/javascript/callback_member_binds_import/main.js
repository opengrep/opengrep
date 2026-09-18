import * as lib from "./a";

export function go() {
  const xs = [source()];
  xs.map(lib.run);
}
