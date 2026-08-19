<?php

// a 'final' modifier on a property hook must reach the generic AST, so that a
// rule can tell a final hook from an ordinary one

//MATCH:
class Yes
{
    public string $a { final set => strtolower($value); }
}

//OK: the hook is not final
class No
{
    public string $b { set => strtolower($value); }
}

//OK: a final get, not a final set
class Other
{
    public string $c { final get => "x"; }
}
