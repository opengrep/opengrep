import type * as types from "./lib";
import * as other from "./other";

export function go() {
  types.run(source());
  other.run(source());
}
