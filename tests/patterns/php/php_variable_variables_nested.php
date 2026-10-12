<?php

// variable variables nest to any depth: '$$$a' is '$' applied to '$$a'

//MATCH:
foo($$$a);

//MATCH:
if (foo($$$a)) {}

//OK: one level less
foo($$a);

//OK: one level more
foo($$$$a);

//OK: a different innermost variable
foo($$$b);

//OK: braces after '$$'
foo($${'a'});
