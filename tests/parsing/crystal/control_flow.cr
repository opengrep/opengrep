def classify(x)
  case x
  when 0
    :zero
  when 1..10
    :small
  else
    :other
  end
end

values = [1, 2, 3]
values.each do |value|
  if value.even?
    puts value
  else
    next
  end
end
