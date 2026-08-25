<?php

// the directive itself must reach the generic AST, so a rule can ask whether
// a file declares something

//MATCH:
declare(strict_types=1);

//OK: a different directive
declare(ticks=1);

//OK: a different value
declare(strict_types=0);

//OK: not a declare at all
foo(1);
