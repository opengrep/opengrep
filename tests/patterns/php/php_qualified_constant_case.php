<?php

// namespace names are case-insensitive, also in the name of a constant

//MATCH:
echo \App\FOO;

//MATCH:
echo \APP\FOO;

//OK: another namespace
echo \Other\FOO;
