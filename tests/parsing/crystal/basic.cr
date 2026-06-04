require "json"

class Greeter
  getter name : String

  def initialize(@name)
  end

  def greet(times = 1)
    times.times do
      puts "hello #{name}"
    end
  end
end

greeter = Greeter.new("Crystal")
greeter.greet(2)

message = <<-TEXT
hello #{greeter.name}
TEXT
