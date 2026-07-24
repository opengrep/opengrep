// An UNRELATED file that happens to define a class of the SAME name with a
// same-arity method.  app.ts never imports it, but its mere presence poisons
// dispatch for Handler.process across the whole project (methods are indexed by
// bare class name), so the legitimate a.ts finding is lost.  This file has no
// sink; it exists only to create the homonym collision.
export default class Handler {
  process(input: string): string {
    return input;
  }
}
