""" AoC 2020 Day 23: Crab Cups
    Author: Chi-Kit Pao

    Commands:
    julia --optimize=3 Day23.jl

    Outputs:
    Question 1: Using your labeling, simulate 100 moves. What are the labels on the cups after cup 1?
    Answer: 97245386
    Question 2: Determine which two cups will end up immediately clockwise of cup 1. What do you get if you multiply their labels together?
    Answer: 156180332979
      3.096432 seconds (31.03 M allocations: 1.529 GiB, 5.86% gc time, 1.31% compilation time)
"""

mutable struct Cups
    values::Vector{Int}
    count::Int
end

function dec(v::Int, len::Int)::Int
    temp = v - 1
    return (temp < 1) ? len : temp
end

# only for part 1
function doMove(cups::Cups)
    current_cup = cups.values[1]
    pick_up = [cups.values[2], cups.values[3], cups.values[4]]

    destination = current_cup
    while true
        destination = dec(destination, cups.count)
        if destination ∉ pick_up
            break
        end
    end

    destination_index = findfirst(map(x -> x == destination, cups.values))
    
    unsafe_copyto!(cups.values, 1, cups.values, 5, destination_index - 5 + 1)
    for i ∈ 1:3
        cups.values[destination_index-4+i] = pick_up[i]
    end
    unsafe_copyto!(cups.values, destination_index, cups.values, destination_index + 1, cups.count - destination_index)
    cups.values[end] = current_cup
end

function part1(input::String)::String
    cups_vector = map(s -> parse(Int, s), split(input, ""))
    cups = Cups(cups_vector, length(cups_vector))
    for _ ∈ 1:100
        doMove(cups)
    end

    index = findfirst(x -> x == 1, cups.values)
    new_vector = Vector{String}()
    for i ∈ (index + 1):(length(cups.values))
        push!(new_vector, string(cups.values[i]))
    end
    for i ∈ 1:(index - 1)
        push!(new_vector, string(cups.values[i]))
    end
    result_vector = join(new_vector, "")
end

mutable struct CupEntry
    id::Int
    prev::Int
    next::Int
end

function get_next(entries::Vector{CupEntry}, id::Int)::Int
    return entries[id].next
end

function part2(input::String)::Int
    # Initialization
    cups_vector = map(s -> parse(Int, s), split(input, ""))
    ENTRY_COUNT = 1000000
    # Move (unsafe_copyto!) is fast even for 1000000 elements. But finding the destination value
    # in the elements takes far too long.
    # It's better to create a vector for item look-up and only manipulate the predecessor and
    # the successor.
    entries = Vector{CupEntry}(undef, ENTRY_COUNT)
    entries[cups_vector[1]] = CupEntry(cups_vector[1], ENTRY_COUNT, cups_vector[2])
    for i ∈ 2:8
        entries[cups_vector[i]] = CupEntry(cups_vector[i], cups_vector[i-1], cups_vector[i+1])
    end
    entries[cups_vector[9]] = CupEntry(cups_vector[9], cups_vector[8], 10)
    entries[10] = CupEntry(10, cups_vector[9], 11)
    for i ∈ 11:(ENTRY_COUNT-1)
        entries[i] = CupEntry(i, i-1, i+1)
    end
    entries[ENTRY_COUNT] = CupEntry(ENTRY_COUNT, ENTRY_COUNT-1, cups_vector[1])

    # Do Moves
    current_cup::Int = cups_vector[1]
    for _ ∈ 1:10000000
        pick_up = [get_next(entries, current_cup)]
        push!(pick_up, get_next(entries, last(pick_up)))
        push!(pick_up, get_next(entries, last(pick_up)))

        destination = current_cup
        while true
            destination = dec(destination, ENTRY_COUNT)
            if destination ∉ pick_up
                break
            end
        end

        next_cup = get_next(entries, last(pick_up))
        next_to_destination = get_next(entries, destination)
        entries[current_cup].next = next_cup
        entries[next_cup].prev = current_cup
        entries[destination].next = pick_up[1]
        entries[pick_up[1]].prev = destination
        entries[pick_up[end]].next = next_to_destination
        entries[next_to_destination].prev = pick_up[end]

        current_cup = next_cup
    end

    id1 = get_next(entries, 1)
    id2 = get_next(entries, id1)
    return id1 * id2
end

function main()
    input::String = "476138259"
    
    println("Question 1: Using your labeling, simulate 100 moves. What are the labels on the cups after cup 1?")
    answer1::String = part1(input)
    println("Answer: $answer1")

    println("Question 2: Determine which two cups will end up immediately clockwise of cup 1. What do you get if you multiply their labels together?")
    answer2 = part2(input)
    println("Answer: $answer2")
end

@time main()
