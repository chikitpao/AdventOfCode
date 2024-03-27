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

@REM ghc --make Day07.hs
@REM Day07.exe

@REM ghc --make Day09.hs
@REM Day09.exe

@REM ghc --make Day11.hs
@REM Day11.exe

@REM ghc --make Day13.hs
@REM Day13.exe

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