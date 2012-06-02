#!/usr/bin/ruby
pattern = Regexp.compile(ARGV[0])
filename = ARGV[1]

File.open(filename, "r") do |file|
  file.each do |line|
    puts "#{file.lineno}: #{line}" if pattern.match(line)
  end
end
  