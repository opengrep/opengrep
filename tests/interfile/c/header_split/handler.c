#include <stdio.h>
#include "handler.h"

static void sink(const char *x) {
    printf("%s", x);
}

void handle(const char *input) {
    // ruleid: test-header-split
    sink(input);
}
