<?php

// a metavariable in the class position of a static call binds a written class
// name, the 'self' keyword and the 'static' keyword alike

class A
{
    public function run($x)
    {
        //MATCH:
        static::foo($x);
        //MATCH:
        self::foo($x);
        //MATCH:
        Cls::foo($x);
        //OK: a different method name
        static::bar($x);
    }
}
