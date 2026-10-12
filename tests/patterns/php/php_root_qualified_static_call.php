<?php

// a qualified name with a leading '\' matches the same name written without
// one, including when the code imports it

namespace App;

use App\Db\Conn;

//MATCH:
$a = \App\Db\Conn::make();

//MATCH: through the import
$b = Conn::make();

//OK: a class of another namespace
$c = \Other\Db\Conn::make();
