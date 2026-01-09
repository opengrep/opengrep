<?php
// PHP 8.1: Readonly properties 
class ReadonlyProperty {
    public readonly int $id;

    public function __construct(int $id) {
        $this->id = $id;
    }
}

// PHP 8.2: Readonly classes 
readonly class ReadonlyClass {
    public int $value;
}
