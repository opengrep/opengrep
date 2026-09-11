<?php
namespace Main;

use Beta\Store;

function go() {
    $s = new Store($_GET["x"]);
    $s->emit();
}
