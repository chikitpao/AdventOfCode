/'
    Aoc 2020, Day 6: Custom Customs
    Author: Chi-Kit Pao

    Commands:
	fbc64 day06.bas
	day06.exe
	(Will open input file input.txt in the same folder.)

	Output:
	Question 1: For each group, count the number of questions to which anyone answered "yes". What is the sum of those count
	s?
	Answer: 6775
	Question 2: For each group, count the number of questions to which everyone answered "yes". What is the sum of those cou
	nts?
	Answer: 3356

'/


FUNCTION CountLetters(a(ANY) AS BOOLEAN) AS INTEGER
	DIM sum AS INTEGER = 0
	FOR i AS INTEGER = 0 TO 25
		IF a(i) THEN
			sum = sum + 1
		END IF
	NEXT i
	CountLetters = sum
END FUNCTION

SUB ClearLetters(a(ANY) AS BOOLEAN, value AS BOOLEAN)
	FOR i AS INTEGER = 0 TO 25
		a(i) = value
	NEXT i
End Sub

DIM s AS STRING
DIM lineCount AS INTEGER = 1
DIM desc AS INTEGER = FREEFILE
DIM hasInput AS BOOLEAN = TRUE
DIM letters1(25) AS BOOLEAN
DIM letters2(25) AS BOOLEAN
DIM sum1 AS INTEGER = 0
DIM sum2 AS INTEGER = 0

ClearLetters letters2(), TRUE
OPEN "input.txt" FOR INPUT ENCODING "ASCII" AS #desc
WHILE NOT EOF(desc)
	LINE INPUT #desc, s
	IF s = "" THEN
		sum1 = sum1 + CountLetters(letters1())
		sum2 = sum2 + CountLetters(letters2())
		ClearLetters letters1(), FALSE
		ClearLetters letters2(), TRUE
		hasInput = FALSE
	ELSE
		hasInput = TRUE
		FOR i AS INTEGER = 1 TO LEN(s)
			letters1(ASC(MID$(s, i, 1)) - ASC("a")) = TRUE
		NEXT i
		FOR i AS INTEGER = ASC("a") TO ASC("z")
			IF INSTR(s, CHR(i)) = 0 THEN
				letters2(i - ASC("a")) = FALSE
			END IF
		NEXT i
	END IF
	lineCount = lineCount + 1
WEND
CLOSE desc

IF hasInput THEN
	sum1 = sum1 + CountLetters(letters1())
	sum2 = sum2 + CountLetters(letters2())
END IF

PRINT "Question 1: For each group, count the number of questions to which anyone answered ""yes"". What is the sum of those counts?"
PRINT "Answer: " & sum1
PRINT "Question 2: For each group, count the number of questions to which everyone answered ""yes"". What is the sum of those counts?"
PRINT "Answer: " & sum2
