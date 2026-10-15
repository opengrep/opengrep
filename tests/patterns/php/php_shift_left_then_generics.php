<?php

// a '<<' shift opens no context, so a later '>>' can still close two type
// arguments

class A
{
    const Y = 4 << 1;

    //MATCH:
    public function f(): Vector<Vector<int>> {
        return $x;
    }

    //OK: another return type
    public function g(): int {
        return $x;
    }
}
