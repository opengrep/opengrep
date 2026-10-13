<?php

// function names are case-insensitive, also with a leading '\'

//MATCH:
\bar();

//MATCH:
\BAR();

//MATCH:
\Bar();

//OK: a pattern with a leading '\' matches only names written with one
bar();
