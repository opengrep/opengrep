<?php

class FieldUser {
    public $name;

    public function __construct() {
        $this->name = "";
    }
    
    public function getProfile() {
        // ruleid: php_constructor_sqli
        $query = "SELECT * FROM users WHERE name = " . $this->name;
        return $query;
    }
}
