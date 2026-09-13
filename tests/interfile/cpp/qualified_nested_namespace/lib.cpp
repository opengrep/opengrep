#include "lib.h"

namespace a {
void handle(const char *input) {
    // ok: qualified-nested-namespace
    sink(input);
}
namespace b {
void handle(const char *input) {
    // ruleid: qualified-nested-namespace
    sink(input);
}
}
}
