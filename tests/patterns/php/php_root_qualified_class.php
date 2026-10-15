<?php

// a leading '\' only says that a class is looked up from the root namespace,
// so a pattern without one matches the name with or without it

//MATCH:
$a = new PDO($dsn);

//MATCH:
$b = new \PDO($dsn);

//OK: a class of another namespace
$c = new \Other\PDO($dsn);
