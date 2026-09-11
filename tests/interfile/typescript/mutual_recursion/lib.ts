function cond(): boolean {
  return false;
}

export function p(x: any): any {
  return q(x);
}

function q(x: any): any {
  return r(source());
}

function r(x: any): any {
  if (cond()) {
    return p(x);
  }
  return x;
}
