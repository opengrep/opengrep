<?php

// PHP 8.0: 'static' is allowed as a return type. It is a type only there, not
// on a property or a parameter, where 'static' is a modifier instead.

class C
{
    public function a(): static
    {
        return new static();
    }

    function b(): static {}

    public static function c(): static {}

    final public function d(): ?static {}

    public function e(): self {}

    public function f(): parent {}

    // 'static' in its other roles must keep working

    public static int $p = 1;
    public static $q;
    protected static ?string $r = null;

    public static function g(): void
    {
        static $counter = 0;
        $h = static function () { return 1; };
    }
}

abstract class A
{
    abstract public function make(): static;
}

interface I
{
    public function build(): static;
}
