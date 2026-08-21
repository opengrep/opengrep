<?php

// PHP 8.5: a promoted constructor property can be declared final. 'final' on
// its own also promotes, and the property is then public.

class C
{
    public function __construct(
        final public int $a,
        final protected string $b,
        final private ?array $c,
        final int $d,
        final public readonly int $e,
        public final int $f,
        final public private(set) int $g,
    ) {}
}

class WithDefaults
{
    public function __construct(
        final public int $a = 1,
        final string $b = "x",
    ) {}
}

class Mixed
{
    public function __construct(
        final public int $promoted,
        int $plain,
        final readonly string $alsoPromoted,
        ...$rest,
    ) {}
}

class WithAttributes
{
    public function __construct(
        #[\Deprecated] final public int $a,
    ) {}
}

// the older spellings must keep working

class Old
{
    public function __construct(
        public int $a,
        protected readonly string $b,
        private(set) int $c,
        int $plain,
    ) {}
}

// 'final' still works where it always did

final class F
{
    final public const K = 1;

    final function m(): void {}
}
