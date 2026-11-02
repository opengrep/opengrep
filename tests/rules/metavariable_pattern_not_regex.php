<?php

// Ensure a pattern-not-regex inside a metavariable-pattern excludes the match
// even when the negated regex and the positive regex match at different offsets
// of $C. Here $C matches getKey (positive) and also contains request (negated),
// so this call must NOT be reported.
// ok: metavariable-pattern-not-regex
sink($req->request->get('x') . $e->getKey('id'));

// $C matches getKey and contains no request, so the negation does not apply.
// ruleid: metavariable-pattern-not-regex
sink($e->getKey('id'));
