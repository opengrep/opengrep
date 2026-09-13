def go
  [source()].map(&:handle)
  [source()].each { |value| process(value) }
end
