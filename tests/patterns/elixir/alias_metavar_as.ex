defmodule AliasMetavarAs do
  # ERROR:
  alias A.B, as: C
  alias A.B
  alias A.D, as: C
end
