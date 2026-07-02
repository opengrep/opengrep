void test_custom_foreach() {
    std::vector<std::string> arr = {source()};
    customForEach<std::string>(arr, [](std::string x) {
        // ruleid: test-hof-taint
        sink(x);
    });
}
