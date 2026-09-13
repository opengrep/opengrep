#include "hof.h"

void sink_callback_direct(char* x) {
    // ruleid: test-hof-taint
    sink(x);
}
