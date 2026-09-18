#include "handler.h"

void handle(const char *input) {
    // ok: overload-by-arity
    sink(input);
}

void handle(const char *input, int flags) {
    // ruleid: overload-by-arity
    sink(input);
}
