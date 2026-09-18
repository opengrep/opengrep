#include "vec.h"

template <typename T>
void Vec<T>::push(T value) {
    // ruleid: template-class-method
    sink(value);
}

void Other::push(const char *value) {
    // ok: template-class-method
    sink(value);
}
