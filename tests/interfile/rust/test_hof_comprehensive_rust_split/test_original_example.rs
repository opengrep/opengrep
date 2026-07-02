fn test_original_example() {
    let history = get_history("name", "owner");
    let arr = vec![history];
    custom_for_each(&arr, |node| {
        let changes = node.clone();
        // ruleid: test-hof-taint
        sink(changes);
    });
}
