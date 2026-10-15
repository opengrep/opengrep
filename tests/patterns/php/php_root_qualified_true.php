<?php

namespace App;

//MATCH:
foo(true);

//MATCH:
foo(\true);

//MATCH:
foo(\TRUE);

//OK: a constant of this namespace, which may have been defined
foo(namespace\true);

//OK: false
foo(\false);

//OK: a constant of another namespace
foo(\Other\true);
