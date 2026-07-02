void test_builtin_for_each() {
    std::vector<std::string> arr = {source()};
    std::for_each(arr.begin(), arr.end(), [](std::string x) {
        // ruleid: test-hof-taint
        sink(x);
    });
}
