def test_original_example():
    history = get_history("name", "owner")
    items = []
    for node in history:
        changes = node.associated_pull_requests.nodes
        # ruleid: test-hof-taint
        sink(changes)

