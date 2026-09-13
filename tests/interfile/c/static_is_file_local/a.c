#include "pub.h"

static void handle_local(const char *x) {
    // ok: static-is-file-local
    sink(x);
}

void handle(const char *input) {
    // ruleid: static-is-file-local
    sink(input);
}
