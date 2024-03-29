del *.exe
del *.hi
del *.o

ghc --make Day02.hs
Day02.exe
@REM Output:
@REM Question 1: What value is left at position 0 after the program halts?
@REM 3516593
@REM Question 2: Find the input noun and verb that cause the program to produce the output 19690720.
@REM 7749

ghc --make Day05.hs
Day05.exe
@REM Question 1: After providing 1 to the only input instruction and
@REM  passing all the tests, what diagnostic code does the program
@REM  produce?
@REM 13547311
@REM Question 2: What is the diagnostic code for system ID 5?
@REM 236453

ghc --make Day07.hs
Day07.exe
@REM Question 1: Try every combination of phase settings on the
@REM  amplifiers. What is the highest signal that can be sent to the
@REM  thrusters?
@REM ([0,3,2,4,1],38500)
@REM Question 2: Try every combination of the new phase settings on
@REM  the amplifier feedback loop. What is the highest signal that
@REM  can be sent to the thrusters?
@REM ([7,5,9,6,8],33660560)

ghc --make Day09.hs
Day09.exe
@REM  Question 1: Once your Intcode computer is fully functional, the
@REM   BOOST program should report no malfunctioning opcodes when run
@REM  in test mode; it should only output a single value, the BOOST
@REM  keycode. What BOOST keycode does it produce?
@REM 3780860499
@REM Question 2: Run the BOOST program in sensor boost mode. What
@REM  are the coordinates of the distress signal?
@REM 33343

ghc --make Day11.hs
Day11.exe
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
Day13.exe
@REM Question 1:How many block tiles are on the screen when the game
@REM  exits?
@REM 306

@REM ghc --make Day15.hs
@REM Day15.exe

@REM ghc --make Day17.hs
@REM Day17.exe

@REM ghc --make Day19.hs
@REM Day19.exe

@REM ghc --make Day21.hs
@REM Day21.exe

@REM ghc --make Day23.hs
@REM Day23.exe

@REM ghc --make Day25.hs
@REM Day25.exe