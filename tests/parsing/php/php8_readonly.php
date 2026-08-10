<?php
// PHP 8.1: Readonly properties 
class ReadonlyProperty {
    public readonly int $id;

    public function __construct(int $id) {
        $this->id = $id;
    }
}

// PHP 8.1: Readonly promoted constructor properties
class ReadonlyPromoted {
    public function __construct(
        public readonly int $id,
        private readonly string $name,
        // the modifiers can come in any order
        readonly protected ?Foo $foo,
        // mixed with regular and variadic parameters
        int $plain = 0,
        string ...$rest,
    ) {}
}

// PHP 8.2: Readonly classes
readonly class ReadonlyClass {
    public int $value;
}
