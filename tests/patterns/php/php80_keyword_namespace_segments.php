<?php

// PHP 8.0: a reserved word may be a later segment of a name

namespace App\Http\List;

use App\Match\Foo;
use App\Match\Foo as Alias;
use App\Enum\{Bar, Baz};

//MATCH:
$a = new Foo();

//MATCH: through an alias
$b = new Alias();

//OK: a different class in a namespace with a reserved word
$c = new Bar();

//OK: a reserved word in a later segment of a different name
$d = new \App\Match\Other();

function f(App\Interface\Foo $x): App\Readonly\Bar
{
    return App\Fn\make($x instanceof App\Class\Foo);
}
