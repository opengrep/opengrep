<?php

class TaintedUser {
    private $key;

    public function __construct($seller) {
        $this->key = source();
    }

    public function props() {
        // ruleid: php_constructor_sqli
        $query = "SELECT * FROM table WHERE name = " . $this->key;
        return $query;
    }
}
