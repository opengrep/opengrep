import { process as p } from "./lib";

export class Runner {
  // Field alias: its Func_info carries lib.ts as the def file.
  handler = p;

  // An ordinary method declared in this file.  Its presence is the whole
  // point of the case: it gives the import-file filter a survivor, so a
  // filter applied per class (rather than per method name) would keep
  // `other` and drop `handler`.
  other(x) {
    return x;
  }
}
