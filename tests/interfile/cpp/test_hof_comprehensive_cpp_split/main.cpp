int main() {
    test_custom_map();
    test_custom_map_builtin();
    test_custom_foreach();
    test_direct_call();
    test_builtin_for_each();
    test_builtin_transform();
    test_original_example();

    // Top-level lambda callback
    // ruleid: test-hof-taint
    auto toplevelSink = [](std::string x) { sink(x); };
    toplevelSink(source());

    // Top-level method HOF (for_each with named callback)
    std::vector<std::string> toplevelItems = {source()};
    std::for_each(toplevelItems.begin(), toplevelItems.end(), toplevelHandler);

    // Top-level user-defined HOF
    customForEach<std::string>(toplevelItems, toplevelHandler);

    return 0;
}
