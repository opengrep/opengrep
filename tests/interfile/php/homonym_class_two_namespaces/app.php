<?php
namespace App;

use Lib\Widget;

function run() {
    $w = new Widget();
    $w->process(taint());
}
