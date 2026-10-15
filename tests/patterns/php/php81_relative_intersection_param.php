<?php

// 'self' and 'parent' may be part of an intersection type

class A extends B
{
    //MATCH:
    public function a(parent&Countable $p) {}

    //MATCH:
    public function b(int $i, parent&Countable $p) {}

    //OK: a by-reference parameter of type 'parent'
    public function c(parent &$p) {}

    //OK: 'self', not 'parent'
    public function d(self&Countable $p) {}

    //OK: a DNF type
    public function e((parent&Countable)|null $p) {}
}
