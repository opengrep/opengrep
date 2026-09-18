#include "taint_split.h"

char* process_data(char* data) {
    // ruleid: simple_c_taint
    return sink(data);
}
