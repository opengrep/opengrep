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
}

enum Plain
{
    #[\Deprecated]
    case A;

    case B;
}
