<?php

// a pattern with a leading '\' asks for the name from the root namespace, so
// it matches only the name written with one

namespace App;

//MATCH:
\exec($a);

//OK: no leading '\'
exec($b);
