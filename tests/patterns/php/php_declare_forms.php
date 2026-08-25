<?php

// code inside a declare block must still reach the generic AST, so a rule
// finds it exactly as it would outside one

declare(strict_types=1) {
    //MATCH:
    function inside()
    {
        return 1;
    }

    //OK: a different function
    function other()
    {
        return 2;
    }
}
