<?php

// constant names are case-sensitive

//MATCH:
echo FOO;

//OK:
echo foo;

//OK:
echo Foo;

//MATCH:
echo \FOO;

//OK:
echo \foo;
