<?php

class Db {
    public static function query($dbh, $sql) {
        // ruleid: static-call-resolution
        return mysqli_query($dbh, $sql);
    }
}
