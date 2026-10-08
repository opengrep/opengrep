<?php

// a label is a statement of its own, so it may also end a block

function a() {
    //MATCH:
    goto done;
    echo 1;
    done:
}

function b() {
    //MATCH:
    goto done;
    done:
    echo 2;
}

function c() {
    try {
    } finally {
        //MATCH:
        goto out;
        out:
    }
}

function d() {
    //OK: a different label
    goto other;
    done:
}
