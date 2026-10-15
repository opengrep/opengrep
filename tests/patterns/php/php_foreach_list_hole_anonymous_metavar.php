<?php

//MATCH:
foreach ($rows as [, $v]) {}

//MATCH:
foreach ($rows as [$k, $v]) {}

//OK:
foreach ($rows as [, $w]) {}
