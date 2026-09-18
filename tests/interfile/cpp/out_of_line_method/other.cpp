#include "store.h"

void Other::send(const char *input) {
    // ok: out-of-line-method
    sink(input);
}
