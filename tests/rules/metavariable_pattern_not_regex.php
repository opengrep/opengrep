<?php

// $C matches /getKey/ but also /request/, so pattern-not-regex must exclude it:
// no match. (Regression: before the fix the negation was silently ignored
// because difference_ranges found no positional overlap, so this line matched.)
sink($req->request->get('x') . $e->getKey('id'));

//ruleid: metavariable-pattern-not-regex
sink($e->getKey('id'));
