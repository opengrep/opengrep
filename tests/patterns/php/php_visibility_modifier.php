<?php

// a visibility modifier in a pattern is honoured, and a method declared
// without one is public, as it is in PHP

class C
{
    //ERROR:
    public function pub($a) { echo $a; }

    //ERROR: no modifier means public
    function plain($a) { echo $a; }

    //ERROR: static, and public by default
    static function stat($a) { echo $a; }

    // OK: private
    private function priv($a) { echo $a; }

    // OK: protected
    protected function prot($a) { echo $a; }
}
