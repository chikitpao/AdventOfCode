{
    Aoc 2020, Day 3: Toboggan Trajectory
    Author: Chi-Kit Pao

    Commands:
    fpc day03.pas
    cat input.txt | ./day03

    Output:
    Question 1: Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?
    Answer: 216
    Question 2: What do you get if you multiply together the number of trees encountered on each of the listed slopes?
    Answer: 6708199680
}


program day03;
var
    line: string;
    lines: Array [0..350] of string;
    lineCount: integer;
    columnCount: integer;
    tempColumnCount: integer;
    answer1: integer;
    answer2: Int64;

function count_trees(lines: Array of string; lineCount: integer; columnCount: integer; right: integer; down: integer): integer;
var
    c: integer;
    r: integer;
    answer: integer;
begin
    c := 0;
    r := 0;
    answer := 0;
    // Pascal doesn't have "step" for "for" keyword, so use "while" instead
    while r < lineCount do
    begin
        if lines[r][c+1] = '#' then
            Inc(answer);
        c := (c + right) mod columnCount;
        r := r + down;
    end;
    count_trees := answer;
end;

begin
    lineCount := 0;
    columnCount := 0;
    tempColumnCount := 0;

    // Read in lines.
    repeat
        readln(line);
        tempColumnCount := Length(line);
        if columnCount = 0 then
            columnCount := tempColumnCount;
        if tempColumnCount > 0 then
        begin
            lines[lineCount] := line;
            Inc(lineCount);
        end;
    until tempColumnCount = 0;

    // Answer 1
    answer1 := count_trees(lines, lineCount, columnCount, 3, 1);

    // Answer 2
    answer2 := answer1;
    answer2 := answer2 * count_trees(lines, lineCount, columnCount, 1, 1);
    answer2 := answer2 * count_trees(lines, lineCount, columnCount, 5, 1);
    answer2 := answer2 * count_trees(lines, lineCount, columnCount, 7, 1);
    answer2 := answer2 * count_trees(lines, lineCount, columnCount, 1, 2);

    writeln('Question 1: Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?');
    write('Answer: ');
    writeln(answer1);
    writeln('Question 2: What do you get if you multiply together the number of trees encountered on each of the listed slopes?');
    write('Answer: ');
    writeln(answer2);
end.