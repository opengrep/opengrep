<?php

// a 'new' used as a constant expression must reach the generic AST as the
// object creation it is, so a rule can find it there

//MATCH:
class Yes
{
    const K = new self();
}

class No
{
    //OK: a different class
    const K = new Other();
}

class Plain
{
    //OK: not a 'new' at all
    const K = 1;
}
