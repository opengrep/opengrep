#include "a.h"
#include "b.h"

using a::handle;

int main() {
    const char *tainted = source();
    handle(tainted);
    return 0;
}
