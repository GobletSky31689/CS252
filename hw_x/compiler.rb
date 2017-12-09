NUM_PAT = /[1-9]\d*|0/
BOOL_PAT = /#[tf]/
IF_PAT = /if /
COMMENT_PAT = /;/
ASSIGN_PAT = /\[/

# Available opcodes for our VM
PRINT_OP = "PRINT"
PUSH_OP = "PUSH"
ADD_OP = "ADD"
SUB_OP = "SUB"
MUL_OP = "MUL"
JMP_OP = "JMP"
JZ_OP = "JZ"
JNZ_OP = "JNZ"
STOR_OP = "STOR"
LOAD_OP = "LOAD"




class AST
  @@mapping = {}
  @@map_counter = 0
  @@label_counter = 0
  attr_accessor :op, :parent, :args
  def initialize(op, parent)
    @op = op
    @parent = parent
    @args = []
  end
  def add_arg(x)
    @args.push(x)
  end
  def to_s
    s = "(" + @op
    @args.each do |arg|
      s += " " + arg.to_s
    end
    s + ")"
  end
  # recursively evaluates the AST, used for the interpreter
  def evaluate
    case @op
    when 'println'
      v = @args[0]
      v = v.evaluate if v.is_a?(AST)
      puts v
    when '+'
      sum = 0
      @args.each do |x|
        x = x.evaluate if x.is_a?(AST)
        sum += x
      end
      return sum
    when '-'
      diff = @args[0]
      diff = diff.evaluate if diff.is_a?(AST)
      args_tail = @args.slice(1, args.length-1)
      args_tail.each do |x|
        x = x.evaluate if x.is_a?(AST)
        diff -= x
      end
      return diff
    when '*'
      prod = 1
      @args.each do |x|
        x = x.evaluate if x.is_a?(AST)
        prod *= x
      end
      return prod
    else
      raise "Unrecognized op '#{@op}'"
    end
  end
  # recursively compiles the AST to bytecode, used for the compiler
  def to_bytecode
    bytecode = []
    case @op
    when 'println'
      comp_arg(@args[0], bytecode)
      bytecode.push(PRINT_OP)
    when 'let'
      comp_arg(@args[1], bytecode)
      @@mapping.store(@args[0], @@map_counter)
      bytecode.push("#{STOR_OP} #{@@map_counter}")
      @@map_counter += 1
      if @args.length > 2
        i = 2
        while not @args[i].is_a?(AST)
          puts @args[i]
          comp_arg(@args[i+1], bytecode)
          @@mapping.store(@args[i+0], @@map_counter)
          bytecode.push("#{STOR_OP} #{@@map_counter}")
          @@map_counter += 1
          i += 2
        end
        @args[i..@args.length].each do |arg|
          comp_arg(arg, bytecode)
        end
      end
    when 'if'
      comp_arg(@args[0], bytecode)
      bytecode.push("#{JZ_OP} fls#{@@label_counter}")
      bytecode.push("tru#{@@label_counter}:")
      comp_arg(@args[1], bytecode)
      bytecode.push("#{JMP_OP} done#{@@label_counter}")
      bytecode.push("fls#{@@label_counter}:")
      comp_arg(@args[2], bytecode)
      bytecode.push("done#{@@label_counter}:")
      @@label_counter += 1
    when '+'
      comp_arg(@args[0], bytecode)
      comp_arg(@args[1], bytecode)
      bytecode.push(ADD_OP)
      if @args.length > 2
        @args[2..@args.length].each do |arg|
          comp_arg(arg, bytecode)
          bytecode.push(ADD_OP)
        end
      end
    when '-'
      comp_arg(@args[0], bytecode)
      comp_arg(@args[1], bytecode)
      bytecode.push(SUB_OP)
      if @args.length > 2
        @args[2..@args.length].each do |arg|
          comp_arg(arg, bytecode)
          bytecode.push(SUB_OP)
        end
      end
    when '*'
      comp_arg(@args[0], bytecode)
      comp_arg(@args[1], bytecode)
      bytecode.push(MUL_OP)
      if @args.length > 2
        @args[2..@args.length].each do |arg|
          comp_arg(arg, bytecode)
          bytecode.push(MUL_OP)
        end
      end
    else
      raise "Unrecognized op '#{@op}'"
    end
    bytecode # Returning bytecode
  end
  private # Unlike Java, this means that *all* of the following functions in AST are private.
  def comp_arg(v, bytecode)
    if v.is_a?(Integer) then
      bytecode.push("#{PUSH_OP} #{v}")
    elsif @@mapping.key? v
      bytecode.push("#{LOAD_OP} #{@@mapping.fetch(v)}")
    else
      bytecode.concat(v.to_bytecode)
    end
  end
end

# Responsible for parsing the source code, either for the interpreter or the compiler
class Parser
  @@vars_seen = []
  def parse(file)
    asts = []
    File.open(file, "r") do |file|
      file.each_line do |ln|
        asts.push(parse_line(ln))
      end
    end
    asts # Returning the ASTs
  end
  private # Unlike Java, this means that *all* of the following functions in AST are private.
  # String -> tokens
  def tokenize_line(line)
    # Adding spaces around parens to make tokenization trivial.
    line = line.gsub(/\(/, ' ( ')
    line = line.gsub(/\)/, ' ) ')
    # Removing spaces around square to make pattern match.
    line = line.gsub(/\[\ */, ' [')
    line = line.gsub(/\ *\]/, '] ')
    line.split
  end
  # [token] -> [AST]
  def parse_line(line)
    tokens = tokenize_line(line)
    ast = nil
    i = 0
    while (i <= tokens.length)
      case tokens[i]
      when '('
        if (tokens[i+1] =~ ASSIGN_PAT) == 0
          @@vars_seen.push(tokens[i+1][1])
          ast.add_arg(tokens[i+1][1])
          ast.add_arg(tokens[i+2][0].to_i)
          i += 2
          while (tokens[i+1] =~ ASSIGN_PAT) == 0
            @@vars_seen.push(tokens[i+1][1])
            ast.add_arg(tokens[i+1][1])
            ast.add_arg(tokens[i+2][0].to_i)
            i += 2
          end
        else
          ast = AST.new(tokens[i+1], ast) # Assuming that we will only receive valid programs
          i += 1 # Skipping an extra token
        end
      when ')'
        if ast.parent then
          ast.parent.add_arg(ast)
          ast = ast.parent
        end
      when COMMENT_PAT
        break # Go to next line
      when BOOL_PAT
        if ast then
          if tokens[i][1] == 't'
            ast.add_arg(1)
          else 
            ast.add_arg(0)
          end
        end
      when NUM_PAT
        if ast then
          ast.add_arg(tokens[i].to_i)
        else
          raise "Top-level numbers are not permitted"
        end
      when /.+/ # If anything else matches (and is at least one char), raise an error
        if ast and @@vars_seen.include? tokens[i]
          ast.add_arg(tokens[i])
        else
          puts tokens
          raise "Unrecognized token: '#{tokens[i]}'"
        end
      end
      i += 1
    end
    ast # Returning the abstract syntax tree
  end
end

# The interpreter, which walks the AST and evaluates as it goes.
# (Not used in this assignment, but available for reference).
class Interpreter
  def initialize
    @parser = Parser.new
  end
  def execute(file)
    asts = @parser.parse(file)
    asts.each do |ast|
      ast.evaluate if ast
    end
  end
end

# Compiles the source code into bytecode format
class Compiler
  def initialize
    @parser = Parser.new
  end
  def compile(scheme_file, bytecode_file)
    asts = @parser.parse(scheme_file)
    File.open(bytecode_file, 'w') do |out|
      asts.each do |ast|
        if ast then
          puts "Parsing #{ast}"
          bytecode = ast.to_bytecode
          out.puts bytecode
        end
      end
    end
  end
end


if ARGV.length < 2
  puts "Usage: ruby compiler.rb <scheme file> <bytecode file>"
  exit 1
end

source = ARGV[0]
output = ARGV[1]

comp = Compiler.new
comp.compile(source, output)


