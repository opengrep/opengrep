<?php
namespace Main;

function go() {
    \App\Svc\Store::run($_GET["x"]);
}
