#include "handlers.h"

void handler(const char *x) {
    // ruleid: function-pointer-variable
    sink(x);
}

void second(const char *x) {
    // ok: function-pointer-variable
    sink(x);
}
