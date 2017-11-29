class Person
  attr_accessor :name
  def initialize(name)
    @name = name
  end

  def make_introduction
    puts "Hi, my name is #{@name}.  Nice to meet you."
  end

  def method_missing(m)
    puts "Didn't understand #{m}"
  end
end

alice = Person.new('Alice')

alice.make_introduction
alice.foo


