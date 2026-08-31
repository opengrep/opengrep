<?php

// a leading '0' means base 8, so all of these are the same number

//ERROR:
foo(017);

//ERROR:
foo(15);

//ERROR: the explicit octal prefix
foo(0o17);

//ERROR:
foo(0x0F);

//ERROR:
foo(0b1111);

// OK: '017' is 15, not 17
foo(17);
