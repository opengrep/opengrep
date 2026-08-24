<?php

// PHP 8.4 property hooks: modifiers, by-reference get, bodiless hooks in
// interfaces and abstract classes, and hooks on promoted properties

interface I
{
    public string $readable { get; }
    public string $both { get; set; }
}

abstract class A
{
    abstract public string $name { get; }
    abstract protected int $count { get; set; }
}

class C
{
    // a hook may be declared final
    public string $username { final set => strtolower($value); }

    public int $twice { final get => 1; }

    // a get hook may return by reference
    public array $items { &get => $this->store; }

    // hooks on a constructor-promoted property
    public function __construct(
        public string $promoted { set => strtolower($value); }
    ) {}
}

// hooks also work on the old 'var' form of a property declaration
class WithVar
{
    var $prop { get => 42; }
}
