let G = "";
let C = "";

export function write() {
  G = source();
}

export function writeClean() {
  C = "safe";
}

export function read() {
  return G;
}

export function readClean() {
  return C;
}
