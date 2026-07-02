<?php

class User {
    private $name;

    public function __construct($userName) {
        $this->name = $userName;
    }
    
    public function getProfile() {
        // ruleid: php_constructor_sqli
        $query = "SELECT * FROM users WHERE name = " . $this->name;
        return $query;
    }
}
