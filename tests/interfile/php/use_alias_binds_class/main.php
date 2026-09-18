<?php
namespace Main;

use App\Store as S;

function go() {
    S::run($_GET["x"]);
}
