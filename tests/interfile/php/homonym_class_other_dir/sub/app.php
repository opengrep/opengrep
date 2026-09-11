<?php
namespace App;

use Sub\Store;

function go() {
    $s = new Store();
    $s->save(taint());
}
