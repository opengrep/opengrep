require_relative 'outer'

# App's include sits in a CLASS body — the well-trodden case.  The
# stress is one level up: Outer's own `include Inner` lives in a
# MODULE body.  For App.new.process to dispatch to Inner#process,
# both levels must register parents.
class App
  include Outer
end

def source
  ENV["INPUT"]
end

def main
  tainted = source
  App.new.process(tainted)
end

main
