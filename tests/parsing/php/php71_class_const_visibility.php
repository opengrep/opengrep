<?php
// PHP 7.1: Class constant visibility
// PHP 8.3: Typed class constants

// Examples from issue #297
class C1 { public const string P = 'Public'; }
class C2 { protected const string P = 'Protected'; }
class C3 { private const string P = 'Private'; }

class ConstantVisibility {
    // Explicit visibility
    public const PUBLIC_CONST = 1;
    protected const PROTECTED_CONST = 2;
    private const PRIVATE_CONST = 3;

    // Default (public)
    const DEFAULT_CONST = 4;

    // Multiple constants with same visibility
    private const A = 1, B = 2;

    // Typed constants (PHP 8.3)
    public const int TYPED_INT = 42;
    private const array TYPED_ARRAY = [1, 2, 3];
}

interface InterfaceConst {
    // Interfaces only support public constants
    public const INTERFACE_CONST = 1;
    const IMPLICIT_PUBLIC = 2;
}
