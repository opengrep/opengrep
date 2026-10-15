<?php

// a DNF type may follow a modifier directly, as in 'private (A&B)|C'

//MATCH:
class A { private (B&C)|D $a; }

//MATCH:
class B { private static (B&C)|D $a; }

//MATCH:
class C { readonly private (B&C)|D $a; }

//MATCH:
class D { private readonly (B&C)|D $a; }

//OK: 'public'
class E { public (B&C)|D $a; }

//OK: a different DNF type
class F { private (B&C)|E $a; }
