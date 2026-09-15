require "./a_b"
require "./b"
require "./run"

def main
  A::B.run(source())
end
