<?php

// PHP 8.3: a class constant may be fetched by a computed name, also in a
// constant expression

class Foo
{
    public const BAR = 'bar';
    public const BA = 'BA';
    public const R = 'R';

    //MATCH:
    public const A = self::{'BA' . 'R'};

    //MATCH:
    public const B = self::{self::BA . self::R};

    //OK: not a concatenation
    public const C = self::{'BAR'};

    //OK: a static name
    public const D = self::BAR;

    //OK: another class
    public const E = Other::{'BA' . 'R'};

    public function f(
        //MATCH:
        string $x = self::{'BA' . 'R'},
    ) {
        //MATCH:
        return self::{$x . 'R'};
    }
}

class Baz extends Foo
{
    //OK: 'parent', not 'self'
    public const F = parent::{'BA' . 'R'};
}
