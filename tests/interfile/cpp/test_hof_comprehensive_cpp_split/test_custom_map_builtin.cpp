void test_custom_map_builtin() {
    std::vector<std::string> arr = {source()};
    customMapBuiltin<std::string>(arr, [](std::string x) {
        // ruleid: test-hof-taint
        sink(x);
        return x;
    });
}
