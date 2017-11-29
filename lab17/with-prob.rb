def with_prob (prob)
  yield if (Random.rand < prob)
end

with_prob 0.42 do
  puts "There is a 42% chance that this code will print"
end



def foo x
  with_prob 0.5 do
    puts "Executing with_prob block"
    return 0
  end
  return x
end

puts "Foo is #{foo 1}"

