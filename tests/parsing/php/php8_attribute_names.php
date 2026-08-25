<?php

// attribute names may be qualified or root-prefixed, and take named arguments

#[Simple]
function f1(): void {}

#[\Deprecated]
function f2(): void {}

#[App\Attributes\Route]
function f3(): void {}

#[\App\Attributes\Route]
function f4(): void {}

#[\Deprecated(message: "use f6() instead", since: "8.4")]
function f5(): void {}

#[Route("/path", name: "home")]
function f6(): void {}

#[First, \Second(1)]
function f7(): void {}

class C
{
    #[\Deprecated]
    const OLD = 1;

    #[Attr(x: 2)]
    public const int TYPED = 2;

    #[\Deprecated(since: "8.4")]
    public function m(#[\SensitiveParameter] string $secret): void {}
}
