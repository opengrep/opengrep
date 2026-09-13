<?php
namespace Main;

function use_store(\App\Svc\Store $s) {
    $s->run($_GET['q']);
}
