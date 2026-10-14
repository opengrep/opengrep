<?php

//OK: a skipped element is not an expression
[, $x] = foo();

//MATCH:
[$a, $x] = foo();
