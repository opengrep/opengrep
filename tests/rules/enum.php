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
// Note that we use bad to make sure we are not finding $s. 
// the idea is that we need to make sure the code is parsed with menir and so the 
// treesitter problem of string parsing does not propagate. See https://github.com/opengrep/opengrep/issues/297

function greet(Status $s, $t): string
{
    return match ($s) {
    // ruleid: test
    Status::Active   => bad("active $t"),
    Status::Inactive => 'Please activate your account.'
  };
}
