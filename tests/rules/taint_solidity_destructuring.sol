contract C {
  // Declaration form: `(T a, T b) = f()`. The entity name is an
  // EPattern holding a PatTuple; before it was lowered through
  // pattern_assign_statements, `a` and `b` were never assigned in the IL.
  function decl_form() public {
    (uint256 a, uint256 b) = taint_source();

    // ruleid: taint-solidity-destructuring
    sink(a);

    // ruleid: taint-solidity-destructuring
    sink(b);
  }

  // Assignment form over already-declared variables. The lhs is a
  // Container(Tuple, ...) of plain names.
  function assign_form() public {
    uint256 c;
    uint256 d;
    (c, d) = taint_source();

    // ruleid: taint-solidity-destructuring
    sink(c);

    // ruleid: taint-solidity-destructuring
    sink(d);
  }

  // Each slot lowers as `pat_i = tmp[i]`, so a clean slot stays clean.
  // This is what keeps the lowering from over-tainting every binding of
  // every destructure.
  function per_slot_precision() public {
    (uint256 g, uint256 h) = (taint_source(), 1);

    // ruleid: taint-solidity-destructuring
    sink(g);

    // ok: taint-solidity-destructuring
    sink(h);
  }

  // Assignment form whose lhs elements are array accesses rather than
  // plain names. These reach AST_to_IL through the expr_to_pattern
  // fallback and now bind to the real lval instead of a throwaway tmp.
  function assign_form_lvals() public {
    uint256[] memory arr = new uint256[](2);
    (arr[0], arr[1]) = taint_source();

    // ruleid: taint-solidity-destructuring
    sink(arr[0]);
  }

  // A single (non-destructuring) declaration keeps its existing lowering.
  function single_binding() public {
    uint256 e = taint_source();

    // ruleid: taint-solidity-destructuring
    sink(e);
  }

  function clean() public {
    uint256 f = 1;

    // ok: taint-solidity-destructuring
    sink(f);
  }
}
