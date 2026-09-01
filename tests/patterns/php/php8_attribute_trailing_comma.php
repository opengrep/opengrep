<?php

// the trailing comma is not part of the arguments, so an attribute carrying
// one matches the same pattern as one written without

//ERROR:
#[Route("/a", name: "a",)]
function a() {}

//ERROR: the same attribute without the trailing comma
#[Route("/a", name: "a")]
function b() {}

// OK: a different argument
#[Route("/c", name: "c",)]
function c() {}
