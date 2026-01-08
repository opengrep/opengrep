<?php
// PHP 8.1: Readonly properties - WORKS
class ReadonlyProperty {
    public readonly int $id;

    public function __construct(int $id) {
        $this->id = $id;
    }
}

// PHP 8.2: Readonly classes - WORKS
readonly class ReadonlyClass {
    public int $value;
}
