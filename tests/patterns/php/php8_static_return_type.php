<?php

// a 'static' return type must reach the generic AST as a type, so a rule can
// tell it apart from 'self' or from any other return type

class A
{
    //MATCH:
    public function make(): static
    {
        return new static();
    }

    //MATCH:
    private function build(): static {}

    //OK: 'self', not 'static'
    public function copy(): self {}

    //OK: an ordinary type
    public function count(): int {}

    //OK: no return type at all
    public function plain() {}

    //OK: 'static' here is a modifier, not a return type
    public static function helper(): int {}
}
