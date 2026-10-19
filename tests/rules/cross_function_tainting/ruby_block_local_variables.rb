# A block-local variable shadows the tainted enclosing variable.
def reads_own_variable
  x = source()
  # ok: ruby_block_local_variables
  [1].each { |a; x| sink(x) }
end

# Without the declaration, the block reads the enclosing variable.
def reads_enclosing_variable
  x = source()
  # ruleid: ruby_block_local_variables
  [1].each { |a| sink(x) }
end

# A write to a block-local variable does not reach the enclosing variable.
def writes_own_variable
  x = "ok"
  [1].each { |a; x| x = source() }
  # ok: ruby_block_local_variables
  sink(x)
end
