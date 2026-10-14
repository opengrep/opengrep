<?php

//OK: a skipped element is not an expression
foreach ($rows as [, $v]) {}

//MATCH:
foreach ($rows as [$k, $v]) {}
