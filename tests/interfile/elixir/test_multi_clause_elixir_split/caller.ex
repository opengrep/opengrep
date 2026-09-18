# Separate module/file: drives the cross-file (remote) calls into each
# multi-clause function of TaintTest with a tainted argument.
defmodule Caller do
  def main() do
    TaintTest.process(source())
    TaintTest.transform(source())
    TaintTest.combine(source(), 42)
    TaintTest.compact(source())
  end
end
