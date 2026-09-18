<?php
namespace Main;

use App\Store;

function make(): Store {
    return new Store();
}

function go() {
    make()->run($_GET["x"]);
}
