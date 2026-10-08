<?php

// keywords as method names and, after a modifier, as aliases in a trait use

class Foo
{
    use TraitA, TraitB {
        TraitA::catch insteadof TraitB;
        TraitA::list as public foreach;
        TraitB::throw as protected;
        namespace as bar;
        try as public attempt;
        foo as private static;
    }

    //MATCH:
    public function run() { return 1; }

    //OK: a different method
    public function stop() { return 2; }
}
