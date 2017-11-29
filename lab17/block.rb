def do_noisy
  puts "About to call block"
  yield
  puts "Just called block"
end

do_noisy do
  puts 3 + 4
end


class Array
  def each_downcase
    self.each do |word|
      yield word.downcase
    end
  end
end

["Alpha", "Beta", "AndSoOn"].each_downcase do |word|
  puts word
end


def iff b
  return if b == false
  yield
end

def test_block
  iff true do
    puts "hello block"
    return
  end
  puts "goodbye block"
end

def iff2 b, lam
  return if b == false
  lam.call
end

def test_lambda
  iff2 true, (lambda do
      puts "hello lambda"
      return
  end)
  puts "goodbye lambda"
end

test_block
test_lambda

