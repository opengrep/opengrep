import { write, writeClean, read, readClean } from "./store";

let G = "";

function readAfterWrite() {
  write();
  // ruleid: global-across-files-js
  sink(read());
}

function readClean() {
  writeClean();
  // ok: global-across-files-js
  sink(readClean());
}

function ownGlobalUntouched() {
  write();
  // ok: global-across-files-js
  sink(G);
}
