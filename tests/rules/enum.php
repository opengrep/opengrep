<?php
// ruleid: enum_rule
enum Status
{
  case Active;
  case Inactive;
  use Logger;

  public function isLive(): bool
  {
    return $this === self::Published;
  }
}
function greet(Status $s, $t): string
{
    return match ($s) {
    // ruleid: test
    Status::Active   => bad("active $t"),
    Status::Inactive => 'Please activate your account.'
  };
}
