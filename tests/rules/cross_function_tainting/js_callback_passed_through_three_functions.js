// A callback passed on through three higher-order functions is called.
function app(f, x) { return f(x); }
function app3(f, g, x) { return f(g, x); }
function app4(f, h, g, x) { return f(h, g, x); }
function snk(y) {
  // ruleid: js_callback_passed_through_three_functions
  sink(y);
}
function main() {
  app4(app3, app, snk, source());
}
