<?php

// PHP 8.5 clone-with: clone takes an optional second argument, an array of
// properties to replace in the copy.

$a = clone($obj, ["p" => 1]);
$b = clone($obj, ["p" => $v, "q" => $w]);
$c = clone($obj, $props);
$d = clone($this->inner, ["p" => 1]);
$e = clone($obj, ["p" => 1,]);
$f = clone(clone($obj, ["p" => 1]), ["q" => 2]);

// the result can be used further

$h = clone($obj, ["p" => 1])->method();
$i = clone($obj, ["p" => 1])->prop;
$j = clone($obj, ["p" => 1])[0];

class C
{
    public function withP(int $p): self
    {
        return clone($this, ["p" => $p]);
    }
}

// the older spellings must keep working

$k = clone $obj;
$l = clone($obj);
$m = (clone $obj)->method();
$n = clone $obj->method();
