<?php

//MATCH:
[, $x] = foo();

//MATCH:
[$a, $x] = foo();

//OK:
[, $y] = foo();
