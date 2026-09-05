require_relative 'lib'

def main
  # The source is injected inside the cycle p -> q -> r -> p and must
  # come around as p's return value.
  # ruleid: mutual-recursion-ruby
  sink(p(0))
end
