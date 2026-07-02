int main() {
    test_custom_foreach();
    test_direct_call();
    test_original_example();

    // Top-level user-defined HOF with named callback
    char* toplevel_items[] = {source()};
    customForEach(toplevel_items, 1, &toplevel_handler);

    // Top-level direct call with function pointer
    directCall(&toplevel_handler);

    return 0;
}
