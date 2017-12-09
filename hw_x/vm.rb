
# Available opcodes for our VM
PUSH_OP = /PUSH (\d+)/ # pushes a number (its argument) on to the stack.
PRINT_OP = /PRINT/ # pops the top number off of the stack and prints it
ADD_OP = /ADD/ # pops the top two elements off the stack, adds them, and puts the result back on to the stack
SUB_OP = /SUB/ # pops the top two elements off the stack, subtracts them, and ...
MUL_OP = /MUL/ # pops the top two elements off the stack, multiplies them, and ...
JMP_OP = /JMP/ # unconditional jump to the specified label
JZ_OP = /JZ/ # pop the top value on the stack.  If it is zero, jump to the specified label.
JNZ_OP = /JNZ/ # pop the top value on the stack.  If it is **not** zero, jump to the specified label.
STOR_OP = /STOR/ # pop the top value on the stack, and store it in the given register.
LOAD_OP = /LOAD/ # get the value from the given register and push it on to the stack.

LABEL_PAT = /[a-zA-Z0-9_]+:/

class VirtualMachine
  def initialize
    @registers = {}
    @stack = []
  end
  def exec(bytecode_file)
    File.open(bytecode_file, 'r') do |file|
      lines = file.readlines
      i = 0
      while (i < lines.size)
        ln = lines[i].strip
        case ln
        when PUSH_OP
          @stack.push(ln.sub(PUSH_OP, '\1').to_i)
        when PRINT_OP
          v = @stack.pop
          puts v
        when ADD_OP
          v1 = @stack.pop
          v2 = @stack.pop
          @stack.push(v2 + v1)
        when JZ_OP
          v1 = @stack.pop
          jmp_to = ln.sub(JZ_OP, '\1').strip
          if v1 == 0
            i += 1
            ln = lines[i].strip
            while not ln == (jmp_to ++ ':') # and i < lines.size
              i += 1
              ln = lines[i].strip
            end
            # puts "Skipped to line:"
            # puts ln
          end
        when JMP_OP
          jmp_to = ln.sub(JMP_OP, '\1').strip
          i += 1
          ln = lines[i].strip
          while not ln == (jmp_to ++ ':') # and i < lines.size
            i += 1
            ln = lines[i].strip
          end
          # puts "Skipped to line:"
          # puts ln
        when STOR_OP
          v1 = @stack.pop
          reg_name = ln.sub(STOR_OP, '\1').strip
          @registers.store(reg_name, v1)
        when LOAD_OP
          reg_name = ln.sub(LOAD_OP, '\1').strip
          v1 = @registers.fetch(reg_name)
          @stack.push(v1.to_i)
        when SUB_OP
          v1 = @stack.pop
          v2 = @stack.pop
          @stack.push(v2 - v1)
        when MUL_OP
          v1 = @stack.pop
          v2 = @stack.pop
          @stack.push(v2 * v1)
        else
          if ln.size > 0 and not (ln =~ LABEL_PAT) == 0
            raise "Unrecognized command: '#{ln}'"
          end
        end
        i += 1
      end
    end
  end
end

if ARGV.length < 1
  puts "Usage: ruby vm.rb <bytecode file>"
  exit 1
end

source = ARGV[0]

vm = VirtualMachine.new
vm.exec(source)


