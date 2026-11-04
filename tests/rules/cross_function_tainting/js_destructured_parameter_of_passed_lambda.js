// A source that matches a destructured parameter of a lambda passed as an
// argument reaches the names that the pattern binds.
import { execSync } from 'child_process';

register(({ handler }) => {
  // ruleid: test-destructured-lambda-parameter
  execSync(`node ${handler}`);
});

register(({ handler }) => {
  // ok: test-destructured-lambda-parameter
  execSync(`node --version`);
});
