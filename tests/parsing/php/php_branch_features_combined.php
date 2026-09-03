<?php

// the constructs this branch touches, in the places they meet each other

namespace App;

enum Suit: string
{
    case Hearts = <<<TXT
        h
        TXT;

    const DEFAULT_SUIT = self::Hearts->value;
}

#[Attr(Suit::Hearts->value, note: <<<TXT
    an attribute argument
    TXT)]
final class Board
{
    public const string LABEL = <<<TXT
        a typed class constant
        TXT;

    public Countable&\ArrayAccess $cell;

    public static $registry;

    public function __construct(
        public readonly Countable&\ArrayAccess $grid,
        private ?string $name = <<<TXT
            a parameter default
            TXT,
        Countable&\ArrayAccess &$byReference = null,
        Countable&\ArrayAccess ...$rest,
    ) {}

    public function &reference(): Countable&\ArrayAccess
    {
        static $memo = <<<TXT
            a static variable
            TXT;

        foreach ([1] as &self::$registry) {}
        $alias = &self::$registry;
        $mask = $memo & self::$registry;
        [$first, &$second] = [1, 2];

        return $this->grid;
    }

    public function nested(callable $f): string
    {
        return <<<OUTER
            before {$f(<<<INNER
            inner body
            INNER)} after
            more of the outer body
            OUTER;
    }

    public function markerLikeText(string $name): string
    {
        return <<<EOT
            hello {$name} EOT is not the marker here
            nor is EOTX
            EOT;
    }
}

const FROM_CONST_EXPR = (new Board(1))->cell;
const FROM_NULLSAFE = (null)?->missing;
