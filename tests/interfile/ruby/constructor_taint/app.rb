require_relative 'store'

def source
  ENV["SECRET"]
end

def main
  tainted = source
  s = Store.new(tainted)
  s.send()
end

main
