<?php

namespace App;

class C
{
    public static function make()
    {
        return static::make();
    }

    public function name()
    {
        return C::class;
    }
}

\foo(1);

\App\Bar::baz(2);
