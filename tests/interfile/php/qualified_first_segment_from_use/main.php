<?php
namespace Main;

use App\Svc;

function go() {
    Svc\Store::run($_GET["x"]);
}
