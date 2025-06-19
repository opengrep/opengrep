<?php

function f($filename) {
    // ruleid: test
    bad("convert $filename");
}
enum Status {
    case Active;
    case Inactive;
}

function greet(Status $s): string {
    // ruleid: test
    return match ($s) {
        Status::Active   => 'Welcome, active user!',
        Status::Inactive => 'Please activate your account.'
    };
}
?>
