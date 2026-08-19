<?php

// PHP 8.4: a 'new' expression with parentheses is directly dereferencable,
// so it no longer has to be wrapped in parentheses of its own.

// --- the forms of dereference -------------------------------------------

$a = new Foo()->method();
$b = new Foo()->prop;
$c = new Foo()::MAX;
$d = new Foo()::class;
$e = new Foo()::$staticProp;
$f = new Foo()::create();
$g = new Foo()::{$dynamic};
$h = new Foo()[0];
$i = new Foo(){'k'};

// --- chaining ------------------------------------------------------------

$j = new Foo()->a()->b()->c();
$k = new Foo()->a->b;
$l = new Foo()[0]->bar();
$m = new Foo()->items[0];
$n = new Foo()::create()->then();

// --- how the class is named ----------------------------------------------

$o = new App\Foo()->bar();
$p = new \App\Foo()->bar();
$q = new $cls()->bar();
$r = new ($factory->cls)()->bar();

class Ctx
{
    public function f()
    {
        $a = new self()->g();
        $b = new static()->g();
        $c = new parent()->g();
        return [$a, $b, $c];
    }
}

// --- constructor arguments ------------------------------------------------

$s = new Foo(1, 2)->bar();
$t = new Foo(new Bar())->baz();
$u = new Foo(1, 2,)->bar();
$v = new Foo(x: 1)->bar();
$w = new Foo(...$args)->bar();
$x = new Foo()->bar(...);

// --- anonymous classes ----------------------------------------------------
// these do NOT need the parentheses, since their brackets precede the body

$y = new class { public function m() {} }->m();
$z = new class () { public function m() {} }->m();
$aa = new class (1, 2) { public function m() {} }->m();

// --- the older spellings must keep working --------------------------------

$ab = new Foo();
$ac = new Foo;
$ad = new Foo->bar();       // still means new (Foo->bar)
$ae = (new Foo())->bar();
$af = (new Foo())::MAX;
$ag = new class {};

// --- '::' after a call, which the same change enables ---------------------

$ah = foo()::MAX;
$ai = foo()::class;
$aj = foo()::bar();
$ak = $obj->m()::MAX;
$al = $obj->m()::bar();
