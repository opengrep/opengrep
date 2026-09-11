<?php
namespace Main;

use App\Store;

class Holder {
    private Store $store;

    public function go($x) {
        $this->store->run($x);
    }
}

function start() {
    $h = new Holder();
    $h->go($_GET["x"]);
}
