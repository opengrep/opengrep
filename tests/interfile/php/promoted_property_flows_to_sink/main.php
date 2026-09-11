<?php
namespace Main;

use App\Holder;

function go() {
    $h = new Holder($_GET["x"]);
    $h->emit();
}
