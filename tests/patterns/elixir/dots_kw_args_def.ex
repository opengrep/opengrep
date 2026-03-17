# MATCH: 1 parameter
def my_function_1(key1: value1) do
  IO.inspect({value1})
end

# MATCH: 3 parameters
def my_function_2(key1: value1, key2: value2, key3: value3) do
  IO.inspect({value1, value2, value3})
end

# MATCH: 3 parameters different order
def my_function_3(key2: value2, key1: value1, key3: value3) do
  IO.inspect({value1, value2, value3})
end
