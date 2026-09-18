<?php

function customForEach($arr, $callback) {
    foreach ($arr as $item) {
        $callback($item);
    }
}
