#include <functional>
void build(int n) {
    std::function<int()> f = [] { return source(); };
    for (int i = 0; i < n; i++) {
        f = [f]() { return f(); };
    }
    // ruleid: closure_capturing_previous_by_value_cpp
    sink(f());
}
