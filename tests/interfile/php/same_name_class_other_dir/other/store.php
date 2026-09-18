<?php
namespace Other;

class Store {
    public function save($data) {
        // ok: same-name-class-other-dir
        sink($data);
    }
}
