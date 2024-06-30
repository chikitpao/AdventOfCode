del *.exe
del *.hi
del *.o

set /A startProgram=25

ghc --make Day02.hs
if %startProgram% LEQ 2 Day02.exe
@REM Output:
@REM Question 1: What value is left at position 0 after the program halts?
@REM 3516593
@REM Question 2: Find the input noun and verb that cause the program to produce the output 19690720.
@REM 7749

ghc --make Day05.hs
if %startProgram% LEQ 5 Day05.exe
@REM Question 1: After providing 1 to the only input instruction and
@REM  passing all the tests, what diagnostic code does the program
@REM  produce?
@REM 13547311
@REM Question 2: What is the diagnostic code for system ID 5?
@REM 236453

ghc --make Day07.hs
if %startProgram% LEQ 7 Day07.exe
@REM Question 1: Try every combination of phase settings on the
@REM  amplifiers. What is the highest signal that can be sent to the
@REM  thrusters?
@REM ([0,3,2,4,1],38500)
@REM Question 2: Try every combination of the new phase settings on
@REM  the amplifier feedback loop. What is the highest signal that
@REM  can be sent to the thrusters?
@REM ([7,5,9,6,8],33660560)

ghc --make Day09.hs
if %startProgram% LEQ 9 Day09.exe
@REM  Question 1: Once your Intcode computer is fully functional, the
@REM   BOOST program should report no malfunctioning opcodes when run
@REM  in test mode; it should only output a single value, the BOOST
@REM  keycode. What BOOST keycode does it produce?
@REM 3780860499
@REM Question 2: Run the BOOST program in sensor boost mode. What
@REM  are the coordinates of the distress signal?
@REM 33343

ghc --make Day11.hs
if %startProgram% LEQ 11 Day11.exe
@REM Question 1: Build a new emergency hull painting robot and run
@REM  the Intcode program on it. How many panels does it paint at
@REM  least once?
@REM 2172
@REM Question 2: After starting the robot on a single white panel
@REM  instead, what registration identifier does it paint on your
@REM  hull?
@REM "               ############      ########################      ######                        ########################      ########################            ############            ######            ######      ##################                  "
@REM "                     ######      ######                        ######                        ######                        ######                        ######            ######      ######            ######      ######            ######            "
@REM "                     ######      ##################            ######                        ##################            ##################            ######                        ########################      ######            ######            "
@REM "                     ######      ######                        ######                        ######                        ######                        ######      ############      ######            ######      ##################                  "
@REM "   ######            ######      ######                        ######                        ######                        ######                        ######            ######      ######            ######      ######                              "
@REM "         ############            ########################      ########################      ########################      ######                              ##################      ######            ######      ######                              "

ghc --make Day13.hs
if %startProgram% LEQ 13 Day13.exe
@REM Question 1:How many block tiles are on the screen when the game
@REM  exits?
@REM 306
@REM Question 2: What is your score after the last block is broken?
@REM Balls: [((20,21),Ball)]
@REM Paddles: [((22,24),Paddle)]
@REM 15328

ghc --make Day15.hs
if %startProgram% LEQ 15 Day15.exe
@REM Question 1: What is the fewest number of movement commands
@REM  required to move the repair droid from its starting position
@REM  to the location of the oxygen system?
@REM 270
@REM Question 2: Use the repair droid to get a complete map of the
@REM  area. How many minutes will it take to fill with oxygen?
@REM 364

ghc --make Day17.hs
if %startProgram% LEQ 17 Day17.exe
@REM Question 1: What is the sum of the alignment parameters for the
@REM  scaffold intersections?
@REM "Answer 1: 7780"
@REM Question 2: After visiting every part of the scaffold at least
@REM  once, how much dust does the vacuum robot report it has
@REM  collected?
@REM "Robot: (42,14,'^')"
@REM "Complete Sequence: L,6,R,8,R,12,L,6,L,8,L,10,L,8,R,12,L,6,R,8,R,12,L,6,L,8,L,8,L,10,L,6,L,6,L,10,L,8,R,12,L,8,L,10,L,6,L,6,L,10,L,8,R,12,L,6,R,8,R,12,L,6,L,8,L,8,L,10,L,6,L,6,L,10,L,8,R,12"
@REM "Answer 2: 1075882"

ghc --make Day19.hs
if %startProgram% LEQ 19 Day19.exe
@REM Question 1: How many points are affected by the tractor beam
@REM  in the 50x50 area closest to the emitter?
@REM "Answer 1: 156"
@REM Question 2: Find the 100x100 square closest to the emitter that
@REM  fits entirely within the tractor beam; within that square, find
@REM  the point closest to the emitter. What value do you get if you
@REM  take that point's X coordinate, multiply it by 10000, then add
@REM  the point's Y coordinate?
@REM "Answer 2: 2610980"

@REM ghc --make Day21.hs
@REM if %startProgram% LEQ 21 Day21.exe

@REM ghc --make Day23.hs
@REM if %startProgram% LEQ 23 Day23.exe

ghc --make Day25.hs
if %startProgram% LEQ 25 Day25.exe