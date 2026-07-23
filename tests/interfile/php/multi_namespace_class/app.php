<?php
namespace App;

use B\Base;

function source() { return $_GET["x"]; }

class Client extends Base {}

function main() {
    $t = source();
    (new Client())->handle($t);
}
