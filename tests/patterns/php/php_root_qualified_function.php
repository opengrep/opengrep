<?php

// a leading '\' only says that a name is looked up from the root namespace,
// so a pattern without one matches the name with or without it

namespace App;

//MATCH:
exec($a);

//MATCH:
\exec($b);

//MATCH: any case
\EXEC($c);

//OK: a function of another namespace
\Other\exec($d);
