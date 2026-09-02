<?php

// 'static' after '::' is a member name; before '::' it is late static binding.
// A rule for one must not find the other.

class K
{
    const static = 1;

    public function m()
    {
        //ERROR:
        $a = K::static;

        // OK: late static binding, not a member called 'static'
        $b = static::make();

        // OK: a different member
        $c = K::self;

        return [$a, $b, $c];
    }
}
