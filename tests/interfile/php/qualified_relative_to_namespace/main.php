<?php
namespace App;

function go() {
    Svc\Store::run($_GET["x"]);
}
