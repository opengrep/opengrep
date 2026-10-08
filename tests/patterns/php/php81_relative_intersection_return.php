<?php

// 'self' and 'parent' may be part of an intersection type

class A extends B
{
    //MATCH:
    public function a(): self&Iterator {}

    //OK: 'parent', not 'self'
    public function c(): parent&Iterator {}

    //OK: no intersection
    public function d(): self {}

    //OK: a DNF type
    public function e(): (self&Iterator)|null {}
}
