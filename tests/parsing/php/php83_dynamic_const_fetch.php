<?php
// PHP 8.3: Dynamic class constant fetch

class Config {
    public const VERSION = '1.0';
    public const DEBUG = true;
}

// Dynamic constant fetch using variable
$name = 'VERSION';
echo Config::{$name};

// Dynamic constant fetch with expression
$constants = ['VERSION', 'DEBUG'];
foreach ($constants as $c) {
    echo Config::{$c};
}

// Nested expression
$prefix = 'VER';
$suffix = 'SION';
echo Config::{$prefix . $suffix};

// Using self and static
class Test {
    public const FOO = 'foo';

    public static function get($name) {
        return self::{$name};
    }

    public static function getStatic($name) {
        return static::{$name};
    }
}
