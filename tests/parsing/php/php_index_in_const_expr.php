<?php

// A constant expression may index into a string, an array literal or another
// constant.

const A = "BAR"[0];
const B = ["x" => 3]["x"];
const C = [1, 2, 3][1];
const D = OTHER[0];
const E = ["a" => ["b" => 1]]["a"]["b"];

class C1
{
    const K = "BAR"[0];

    public $p = [1, 2][0];

    public function m($a = [1, 2][0], $b = "xy"[1]) {}
}

function f($a = ["x" => 1]["x"]) {}

#[Attr(["a", "b"][0])]
function g() {}

// indexing in expression position must keep working

$x = $arr[0];
$y = "BAR"[0];
$z = foo()[1];

// and so must plain constant expressions

const F = [1, 2, 3];
const G = "BAR";
const H = OTHER;
