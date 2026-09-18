#include "hof.h"

void sink_callback(char* x) {
    // ruleid: test-hof-taint
    sink(x);
}
