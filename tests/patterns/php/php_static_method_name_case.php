<?php

// class and method names are case-insensitive

//MATCH:
Foo::bar();

//MATCH:
FOO::BAR(1);

//MATCH:
foo::Bar();

//OK: another method
Foo::baz();
