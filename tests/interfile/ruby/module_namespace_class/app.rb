require_relative 'lib'

class Client < Svc::Base
end

def source
  ENV["INPUT"]
end

def main
  tainted = source
  Client.new.handle(tainted)
end

main
