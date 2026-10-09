<?php

// what follows __halt_compiler() is data, also when the call ends with a
// closing tag, as in a phar stub

//MATCH:
eval($code);

__HALT_COMPILER(); ?>
<?php eval($payload); ?>
data { " ' /* $$$
