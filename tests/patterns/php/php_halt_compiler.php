<?php

// what follows '__halt_compiler();' is data, not code

//MATCH:
eval($code);

function f($code)
{
    //MATCH:
    return eval($code);
}

__halt_compiler();
<?php eval($payload); ?>
eval($other);
"unterminated { /* $$$
