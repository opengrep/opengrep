def handle(%User{name: name} = user) do
  #ruleid: taint-pattern-as 
  sink(name)

  #ruleid: taint-pattern-as 
  sink(user)
end
