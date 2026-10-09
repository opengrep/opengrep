<?php

// __halt_compiler() stops lexing only at the top level; elsewhere a closing
// tag after it leaves PHP code as usual

if ($x) { __halt_compiler() ?>
<p>it's html</p>
//OK: inline HTML, not code
<p>eval($html)</p>
<?php }

//MATCH:
eval($code);
