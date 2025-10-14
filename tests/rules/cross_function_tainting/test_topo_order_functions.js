// Test topological ordering: helper function defined after caller

function caller() {
  const userInput = source();
  // ruleid: test_topo_order_functions
  sink(helper(userInput));
}

// Helper function defined after caller - should be analyzed first
function helper(data) {
  return data; // Returns tainted data
}

caller();
