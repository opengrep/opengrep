def query
  1
end


def shadows
  # Assignment makes `query` method-local: it must NOT resolve to the
  # top-level `def query` above.
  query = make()
  sink(query)
end


def rebinds_in_block
  # A block closes over enclosing locals: the assignment inside the block
  # rebinds `acc`, it does not declare a fresh block-local.
  acc = seed()
  [1, 2].each do |item|
    acc = item
  end
  sink(acc)
end
