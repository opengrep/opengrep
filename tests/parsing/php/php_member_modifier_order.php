<?php

// Modifiers may appear in any order before a constant, a property or a
// method, and which member kind follows is what tells them apart.

class C
{
    final public const A = 1;
    public final const B = 2;
    final const C = 3;
    public const D = 4;
    const E = 5;

    final public function m1(): void {}
    public final function m2(): void {}
    final function m3(): void {}
    public function m4(): void {}
    function m5(): void {}

    abstract public function m6(): void;
    public abstract function m7(): void;

    public static function m8(): void {}
    static public function m9(): void {}

    public readonly int $p1;
    readonly public int $p2;
    public static int $p3 = 0;
    static public int $p4 = 0;
    public int $p5 = 0;
    var $p6;
}
