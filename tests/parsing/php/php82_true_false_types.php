<?php

// PHP 8.2: 'true' and 'false' are types in their own right, most often seen
// as the failure half of a union such as 'array|false'.

function a(): array|false {}
function b(): string|false {}
function c(): array|true {}
function d(): false {}
function e(): true {}
function f(): ?false {}
function g(): DateTime|false {}
function h(): array|false|null {}

function i(false $v) {}
function j(true $v) {}
function k(array|false $v) {}
function l(false $v = null) {}

class C
{
    public array|false $p;
    public false $q;
    private string|false $r = false;

    public function m(): self|false {}

    public array|false $hooked {
        get => $this->hooked;
    }
}

interface I
{
    public function n(): array|false;
}

// type names are case-insensitive, as they are for 'array'

function q(): array|FALSE {}
function r(): ARRAY|False {}
function s(): TRUE {}
function t(FALSE $v) {}

// 'null' as a type already worked

function o(): array|null {}
function p(): ?array {}

// and the boolean literals must still be literals in expression position

$x = true;
$y = false;
$z = TRUE;
if (false) {
}
$w = [true, false];
