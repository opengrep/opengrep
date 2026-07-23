require_relative 'widget_a'
require_relative 'widget_b'
require_relative 'decoy'

class Sub < Widget
end

def source
  ENV["X"]
end

def main
  t = source
  Sub.new.run(t)
end

main
