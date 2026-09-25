namespace {
  function helper($x) {
    // ruleid: hack_rooted_name_in_namespace_block
    sink($x);
  }
}

namespace App {
  function helper($x) {
    // ok: hack_rooted_name_in_namespace_block
    sink($x);
  }

  function main() {
    \helper(source());
  }
}
