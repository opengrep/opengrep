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

function main() {
    $taintedInput = source();
    $user = new User($taintedInput);
    $result = $user->getProfile();
    
    // Test field assignment taint flow
    $taintedInput2 = source();
    $fieldUser = new FieldUser();
    $fieldUser->name = $taintedInput2;
    $fieldResult = $fieldUser->getProfile();
    
    return $result;
}

?>