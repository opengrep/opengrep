<?php
namespace Main;

function go() {
    array_map('App\handle', [$_GET["x"]]);
}
