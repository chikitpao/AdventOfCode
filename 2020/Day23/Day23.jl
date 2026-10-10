""" AoC 2020 Day 23: Crab Cups
    Author: Chi-Kit Pao

    Commands:
    julia --optimize=3 Day23.jl

    Outputs:
    Question 1: Using your labeling, simulate 100 moves. What are the labels on the cups after cup 1?
    Answer: 97245386
      0.055888 seconds (45.73 k allocations: 2.296 MiB, 97.86% compilation time)
"""

mutable struct Cups
    values::Vector{Int}
    count::Int
end

function dec(v::Int, len::Int)
    temp = v - 1
    return (temp < 1) ? len : temp
end

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
    new_vector = Vector{Int}()
    for i ∈ 5:destination_index
        push!(new_vector, cups.values[i])
    end
    append!(new_vector, pick_up)
    for i ∈ (destination_index+1):(cups.count)
        push!(new_vector, cups.values[i])
    end
    push!(new_vector, current_cup)
    cups.values = new_vector
end

function part1(values::Vector{Int})
    index = findfirst(x -> x == 1, values)
    new_vector = Vector{String}()
    for i ∈ (index + 1):(length(values))
        push!(new_vector, string(values[i]))
    end
    for i ∈ 1:(index - 1)
        push!(new_vector, string(values[i]))
    end
    return join(new_vector, "")
end

function main()
    input = "476138259"
    cups_vector = map(s -> parse(Int, s), split(input, ""))
    cups = Cups(cups_vector, length(cups_vector))
    for _ ∈ 1:100
        doMove(cups)
    end
    answer1 = part1(cups.values)

    println("Question 1: Using your labeling, simulate 100 moves. What are the labels on the cups after cup 1?")
    println("Answer: $answer1")
end

@time main()
