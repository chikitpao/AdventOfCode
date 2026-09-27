=begin

    AoC2020, Day 14: Docking Data
    Author: Chi-Kit Pao

    Commands:
    ruby day14.php

    Output:
    Question 1: What is the sum of all values left in memory after it completes?
    Answer: 17765746710228
    Question 2: What is the sum of all values left in memory after it completes?
    Answer: 4401465949086

    Time usage shown via command "time":
    real	0m0,058s
    user	0m0,051s
    sys	0m0,008s

=end

def part1(lines)
    memory = Hash.new
    or_mask = 0
    and_mask = 2**36 - 1
    lines.each do |line|
        # Line examples:
        # mask = 111X000100XX1X01X1X10X01X11101100010
        # mem[22535] = 42768
        line_parts = line.split(" = ", -1)
        len1 = line_parts[1].length
        if line_parts[0] == "mask"
            or_mask = 0
            and_mask = 2**36 - 1
            mask = line_parts[1]
            (0..35).each do |counter|
                c = mask[counter, 1]
                case c
                when '0'
                    and_mask = and_mask - (1 << (35 - counter))
                when '1'
                    or_mask = or_mask | (1 << (35 - counter))
                end
            end
        else
            index = Integer(line_parts[0][4, line_parts[0].length - 5])
            value = Integer(line_parts[1])
            value1 = value | or_mask
            value2 = value1 & and_mask
            memory[index] = value2
        end
    end
    return memory.values.sum
end

def part2(lines)
    memory = Hash.new
    or_mask = 0
    and_mask = 2**36 - 1
    floating_bit_values = []
    floating_values = []
    lines.each do |line|
        # Line examples:
        # mask = 111X000100XX1X01X1X10X01X11101100010
        # mem[22535] = 42768
        line_parts = line.split(" = ", -1)
        len1 = line_parts[1].length
        if line_parts[0] == "mask"
            or_mask = 0
            and_mask = 2**36 - 1
            floating_bit_values = []
            floating_values = []
            mask = line_parts[1]
            (0..35).each do |counter|
                c = mask[counter, 1]
                case c
                when '1'
                    or_mask = or_mask | (1 << (35 - counter))
                when 'X'
                    and_mask = and_mask - (1 << (35 - counter))
                    floating_bit_values.push(1 << (35 - counter))
                end
            end
            if floating_bit_values.empty?
                floating_values = [0]
            else
                # Power set of bit values
                (0..(2**(floating_bit_values.length)-1)).each do |i|
                    sum = 0
                    (0..(floating_bit_values.length)).each do |j|
                        v = 1 << j
                        if i & v != 0
                            sum = sum + floating_bit_values[j]
                        end
                    end
                    floating_values.push(sum)
                end
            end

        else
            index = Integer(line_parts[0][4, line_parts[0].length - 5])
            value = Integer(line_parts[1])
            index1 = index | or_mask
            index2 = index1 & and_mask
            floating_values.each do |floating_value|
                memory[index2 + floating_value] = value
            end

        end
    end
    return memory.values.sum
end

def main
    lines = []
    File.readlines('input.txt', chomp: true).each do |line|
        lines.append(line)
    end

    answer1 = part1(lines)
    puts "Question 1: What is the sum of all values left in memory after it completes?"
    puts "Answer: #{answer1}"

    answer2 = part2(lines)
    puts "Question 2: What is the sum of all values left in memory after it completes?"
    puts "Answer: #{answer2}"

end

main()





