import { first } from "./a";
import { second } from "./b";

var run = first;
var run = second;

export function go() {
  run(source());
}
