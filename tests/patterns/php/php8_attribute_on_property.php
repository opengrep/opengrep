<?php

// an attribute on a property must reach the generic AST, so a rule can find
// properties carrying it

//MATCH:
class Yes
{
    #[\Deprecated]
    public int $a;
}

class No
{
    //OK: no attribute
    public int $a;
}

class Other
{
    //OK: a different attribute
    #[\Something]
    public int $a;
}
