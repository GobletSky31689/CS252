s = "Hi, I'm Larry; this is my" +
" brother Darryl, and this" +
" is my other brother Darryl."
s.sub(/Larry/,'Laurent')
puts s
s.sub!(/Larry/,'Laurent')
puts s
puts s.sub(/brother/, 'frère')
puts s.gsub(/brother/, 'frère')