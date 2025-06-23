<?php

function f($filename)
{
  // ruleid: test
  bad("convert $filename");
}
// ruleid: enum_rule
enum Status
{
  case Active;
  case Inactive;
  use Logger;
  public const DEFAULT = self::Draft;

  public function isLive(): bool
  {
    return $this === self::Published;
  }
}
function greet(Status $s): string
{
  // ruleid: test
  return match ($s) {
    Status::Active   => 'Welcome, active user!',
    Status::Inactive => 'Please activate your account.'
  };
}
