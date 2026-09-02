<?php

// a metavariable may stand for a type on either side of an intersection where
// no by-reference parameter could be meant instead

//ERROR:
class K { public A&B $p; }

//ERROR:
class L { public A&C $p; }

// OK: a property whose first type differs
class M { public Z&B $p; }
