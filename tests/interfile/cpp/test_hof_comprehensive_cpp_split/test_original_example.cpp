void test_original_example() {
    std::string history = getHistory("name", "owner");
    std::vector<std::string> vec = {history};
    customForEach<std::string>(vec, [](std::string node) {
        std::string changes = node;
        // ruleid: test-hof-taint
        sink(changes);
    });
}
