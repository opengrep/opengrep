#include "store.h"

void Store::send(const char *input) {
    // ruleid: this-arrow-method
    sink(input);
}

void Other::send(const char *input) {
    // ok: this-arrow-method
    sink(input);
}
