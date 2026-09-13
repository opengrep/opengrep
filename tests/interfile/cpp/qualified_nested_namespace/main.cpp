#include "lib.h"

namespace a {
void run() {
    const char *tainted = source();
    b::handle(tainted);
}
}
