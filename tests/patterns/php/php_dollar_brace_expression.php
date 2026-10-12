<?php

// in "${...}" a name followed by '}' or '[' is a variable's name, and
// anything else is an expression whose value is the name of the variable to
// read, as ${...} does outside a string

//MATCH:
echo "${substr($name, 0, 2)}";

// in a heredoc
echo <<<EOT
    //MATCH:
    value: ${substr($name, 1)}
    EOT;

//MATCH: outside a string
echo ${substr($name, 0, 2)};

//OK: the call itself, not a variable it names
echo substr($name, 0, 2);

//OK: the variable $substr
echo "${substr}";

//OK: an element of the variable $substr
echo "${substr[0]}";
