<?php

// 'static' can be part of a union return type

class A
{
    //MATCH:
    public function make(): static|null
    {
        return null;
    }

    //OK: no 'null'
    public function build(): static|array {}

    //OK: 'self', not 'static'
    public function copy(): self|null {}

    //OK: 'static' alone
    public function plain(): static {}

    //OK: a class constant, not a return type
    public const static|null C = null;
}
