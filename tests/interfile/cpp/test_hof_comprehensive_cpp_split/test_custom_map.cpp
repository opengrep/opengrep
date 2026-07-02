void test_custom_map() {
    std::vector<std::string> arr = {source()};
    customMap<std::string>(arr, [](std::string x) {
        // ruleid: test-hof-taint
        sink(x);
        return x;
    });
}
