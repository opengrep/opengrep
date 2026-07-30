export function process(data) {
  // Reached via Runner.handler, a class-body field alias bound to THIS
  // function.  The alias's def file is lib.ts, not the class's runner.ts,
  // so import-file narrowing must not filter it out.
  // ruleid: class-alias-with-method
  sink(data);
}
