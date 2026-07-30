<?php
require_once "widget_b.php";

function run() {
    $w = new Widget();
    $w->process(taint());
}
