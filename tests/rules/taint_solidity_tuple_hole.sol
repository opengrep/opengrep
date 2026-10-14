contract C {
  // a skipped component keeps the later components at their slot

  function assign_first_hole() public {
    uint256 b;
    (, b) = (taint_source(), 1);
    // ok: taint-solidity-tuple-hole
    sink(b);
  }

  function assign_first_hole_tainted() public {
    uint256 b;
    (, b) = (1, taint_source());
    // ruleid: taint-solidity-tuple-hole
    sink(b);
  }

  function decl_first_hole() public {
    (, uint256 b) = (taint_source(), 1);
    // ok: taint-solidity-tuple-hole
    sink(b);
  }

  function decl_first_hole_tainted() public {
    (, uint256 b) = (1, taint_source());
    // ruleid: taint-solidity-tuple-hole
    sink(b);
  }

  function assign_trailing_hole() public {
    uint256 a;
    (a, ) = (taint_source(), 1);
    // ruleid: taint-solidity-tuple-hole
    sink(a);
  }
}
