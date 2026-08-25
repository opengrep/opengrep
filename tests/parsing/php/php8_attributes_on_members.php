<?php

// PHP 8.0 attributes are allowed on properties and on enum cases, as well as
// on the members that already accepted them.

class C
{
    #[\Deprecated]
    public int $a = 1;

    #[Attr(x: 1)]
    protected static ?string $b = null;

    #[First, \Second]
    private array $c = [];

    #[\Deprecated]
    public const K = 1;

    #[\Deprecated]
    public function m(#[\SensitiveParameter] string $secret): void {}

    public function __construct(
        #[\SensitiveParameter] public string $promoted,
    ) {}
}

enum Suit: string
{
    #[\Deprecated]
    case Hearts = 'H';

    #[Attr]
    case Spades = 'S';

    case Clubs = 'C';

    // an attributed member following the cases

    #[\Deprecated]
    public const K = 1;

    #[\Deprecated]
    public function label(): string
    {
        return $this->value;
    }
}

enum Plain
{
    #[\Deprecated]
    case A;

    case B;

    #[\Deprecated]
    public function f(): void {}

    // a case may also follow a member

    case C;
}

// every attribute form is allowed on a member following a case

interface HasLabel {}

enum Rich: string implements HasLabel
{
    case A = 'a';

    #[First]
    #[\Second]
    public function m1(): void {}

    #[Attr(1, b: 2)]
    final public function m2(): void {}

    #[\Foo\Bar]
    public static function m3(): void {}

    #[\Deprecated]
    public const K = 1;

    case B = 'b';
}

// an enum need not have any case at all

enum NoCases
{
    #[\Deprecated]
    public function f(): void {}
}
