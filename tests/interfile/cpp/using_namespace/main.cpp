#include "a.h"
#include "b.h"

using namespace a;

int main() {
    const char *tainted = source();
    handle(tainted);
    return 0;
}
