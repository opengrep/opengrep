<?php

// PHP 8.1: 'new' is allowed in an initializer, i.e. wherever a constant
// expression is expected.

const S = new S();
const T = new Foo(1, "x");
const U = new \App\Thing();
const V = new Foo;

#[SomeAttribute(new stdClass)]
#[Other(new Foo(1))]
function attributed() {}

function withDefaults(
    $a = new stdClass(),
    $b = new Foo(1, "x"),
) {}

class C
{
    public $prop = new stdClass();

    const K = new Foo();

    public function m($o = new stdClass()) {}
}

function statics()
{
    static $x = new stdClass();
    static $y = new class {};
    static $z = new stdClass(...[0]);
}

// self, parent and static are keywords rather than plain class names, so they
// need spelling out separately

class Keywords extends Base
{
    const K = new self();
    const L = new parent();

    public $p = new self();

    public function m(
        $a = new self(),
        $b = new parent(1, 2),
    ) {}

    #[Attr(new self())]
    public function n()
    {
        static $x = new static;
        static $y = new static();
        static $z = new self(1);
    }
}

// ordinary 'new' in expression position must keep working

$a = new Foo();
$b = new Foo(1, 2);
$c = new class {};
$d = new Foo()->bar();

$e = new self();
$f = new static();
$g = new parent();
