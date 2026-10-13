<?php

// namespace and function names are case-insensitive, also in a qualified name

//MATCH:
\Ns\bar();

//MATCH:
\NS\BAR();

//MATCH:
\ns\Bar();

//OK: another namespace
\Other\bar();
