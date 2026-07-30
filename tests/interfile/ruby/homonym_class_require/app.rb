require_relative "widget_b"

def run
  w = Widget.new
  w.process(taint())
end
