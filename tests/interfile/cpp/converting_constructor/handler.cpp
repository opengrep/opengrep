#include "conv.h"

void handle(Conv c, const char *input) {
    // ruleid: converting-constructor
    sink(input);
}

void drop(Conv c, const char *input) {
    // ok: converting-constructor
    sink("");
}
