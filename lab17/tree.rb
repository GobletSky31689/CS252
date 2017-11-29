class Tree
  attr_accessor :value, :left, :right
  def initialize(value, left=nil, right=nil)
    @value = value
    @left = left
    @right = right
  end

  def method_missing(m)
    path = m.to_s.split('_')
    result = self
    path.each { |x| result==nil || result = result.send(x)}
    return result.value if result!=nil else nil
  end

  def each_node
    left = @left
    right = @right
    yield(@value)
    if @left != nil
      left.each_node do |v|
        yield(v)
      end
    end
    if @right != nil
      right.each_node do |v|
        yield(v)
      end
    end
  end
end

my_tree = Tree.new(42,
                   Tree.new(3,
                            Tree.new(1,
                                     Tree.new(7,
                                              Tree.new(22),
                                              Tree.new(123)),
                                     Tree.new(32))),
                   Tree.new(99,
                            Tree.new(81)))

my_tree.each_node do |v|
  puts v
end

arr = []
my_tree.each_node do |v|
  arr.push v
end
p arr

p "Getting nodes from tree"
p my_tree.left_left
p my_tree.right_left
p my_tree.left_left_right
p my_tree.left_left_left_right
p my_tree.right_right_left


