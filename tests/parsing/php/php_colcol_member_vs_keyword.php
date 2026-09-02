<?php

// after '::' a word is a member name, never the construct it spells: 'C::static'
// is a constant called 'static', not late static binding. Before '::' those
// same words keep their meaning.

class K
{
    const self = 1;
    const static = 2;
    const __CLASS__ = 3;

    public function array() {}

    public function m()
    {
        // member names
        $a = K::self;
        $b = K::static;
        $c = K::__CLASS__;
        $d = $this->array();
        $e = K::array();

        // the constructs, which are unaffected
        $f = self::MAX;
        $g = static::make();
        $h = parent::m();
        $i = static::class;
        $j = self::class;
        $k = __CLASS__;
        $l = array(1, 2);

        return [$a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l];
    }
}
