<?php

// namespace and class names are case-insensitive, also in a use declaration

//MATCH:
use App\Foo;

//MATCH:
use app\foo;

//OK: another class
use App\Bar;
