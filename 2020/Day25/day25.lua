--[[

    AoC2020, Day 25: Combo Breaker
    Author: Chi-Kit Pao

    Commands:
    cat input.txt | lua5.4 day25.lua

    Output:
    Question: What encryption key is the handshake trying to establish?
    Answer: 3803729

    Time usage shown via command "time".
    real	0m0,973s
    user	0m0,975s
    sys	0m0,001s

--]]

local function powermod(x, p, m)
    if m <= 1 then
        return 0
    end
    x = x % m
    local result = 1
    local temp_p = p
    while temp_p > 0 do
        if temp_p % 2 == 1 then
            result = (result * x) % m
        end
        x = (x * x) % m
        temp_p = temp_p // 2
    end
    return result
end

function main()
    p = 7
    m = 20201227
    x = 1
    v = 7
    a = 0
    b = 0
    c = 0
    -- read two numbers from stdin
    pk1 = io.read("*n")
    pk2 = io.read("*n")
    while true do
        x = x + 1
        v = v * p
        v = v % m
        if a == 0 and v == pk1 then
            a = x
        end
        if b == 0 and v == pk2 then
            b = x
        end
        if a ~= 0 and b ~= 0 then
            break
        end
    end
    answer = powermod(p, a * b, m)
    print("Question: What encryption key is the handshake trying to establish?")
    print("Answer: " .. answer)
end

main()
