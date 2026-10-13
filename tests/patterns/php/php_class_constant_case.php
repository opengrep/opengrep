<?php

// class constant names are case-sensitive, class names are not

//MATCH:
echo Foo::BAR;

//MATCH:
echo FOO::BAR;

//OK:
echo Foo::bar;
