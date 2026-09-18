<?php
namespace App;

function helper($x) {
    // ok: rooted-global-function
    sink($x);
}

function go() {
    \helper($_GET["x"]);
}
