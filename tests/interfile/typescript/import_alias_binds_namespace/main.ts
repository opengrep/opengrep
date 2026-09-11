import { Svc } from "./lib";
import S = Svc.Inner;

export function go() {
  S.run(source());
}
