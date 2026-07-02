async function test_original_example() {
  const history = await getHistory("name", "owner");
  const items = history.flatMap((node) => {
    const changes = node.associatedPullRequests.nodes;
    // ruleid: test-hof-taint
    return sink(changes);
  });
}
