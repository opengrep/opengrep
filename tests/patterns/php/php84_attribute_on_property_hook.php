<?php

// an attribute on a property hook must reach the generic AST alongside the
// getter/setter marker, so a rule can find hooks carrying it

//MATCH:
class Yes
{
    public string $a {
        #[\Deprecated]
        get => $this->a;
    }
}

class No
{
    //OK: no attribute on the hook
    public string $a {
        get => $this->a;
    }
}

class Other
{
    //OK: a different attribute
    public string $a {
        #[\Something]
        get => $this->a;
    }
}
