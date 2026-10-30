<?php
// A closure passed as the argument that a sink matches: the closure itself is
// the sink argument, and a variable it captures is not data passed there.

function search($request) {
  $column = $request->input('column');
  $rows = query(function ($q) use ($column) {
    // ruleid: test-closure-argument-of-sink
    return where($column);
  });
}

function search_by_id($request) {
  $column = $request->input('column');
  $rows = query(function ($q) use ($column) {
    // ok: test-closure-argument-of-sink
    return where('id');
  });
}

function count_rows($request) {
  $column = $request->input('column');
  // ok: test-closure-argument-of-sink
  $rows = query(function ($q) use ($column) {
    return $column;
  });
}
