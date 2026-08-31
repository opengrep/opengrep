<?php

// separators are not part of the value, so a literal carrying them matches an
// equal literal written any other way

//ERROR:
foo(0x1_C);

//ERROR: the same value in decimal
foo(28);

//ERROR: and in binary
foo(0b1_1100);

// OK: a different value
foo(29);

// OK: a different value
foo(0x1_D);
