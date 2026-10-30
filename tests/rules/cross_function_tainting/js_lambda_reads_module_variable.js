// A lambda defined at module level reads the module's variables where it is
// used, as a lambda in a function reads that function's locals.
const tainted = source();
const clean = "safe";
arr.forEach(() => {
  // ruleid: js_lambda_reads_module_variable
  sink(tainted);
});
arr.forEach(() => {
  // ok: js_lambda_reads_module_variable
  sink(clean);
});
