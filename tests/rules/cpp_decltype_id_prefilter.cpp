template<typename T>
void f(T e) {
    // ruleid: cpp-decltype-id
    decltype(e)::foo x;
}
