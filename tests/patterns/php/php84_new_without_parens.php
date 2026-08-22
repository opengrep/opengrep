<?php

// the parenthesized and unparenthesized spellings mean the same thing, so a
// rule written either way must match both

//MATCH:
$a = new Foo()->bar();

//MATCH:
$b = (new Foo())->bar();

//OK: different method
$c = new Foo()->other();

//OK: different class
$d = new Bar()->bar();

//OK: a property, not a call
$e = new Foo()->bar;

//OK: 'new Foo->bar()' still means new (Foo->bar)
$f = new Foo->bar();
