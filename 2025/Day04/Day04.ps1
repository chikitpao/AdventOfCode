<# 
    Advent of Code 2025
    Day 4: Printing Department
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day04.ps1"
#>

class Maze {
    [int]$Width = 0
    [int]$Height = 0
    [System.Collections.ArrayList]$Rows = [System.Collections.ArrayList]::new()
}

function CheckValidPos()
 {
    param (
        [Maze]$Maze,
        [int]$Row,
        [int]$Column
    )

    if ($Row -lt 0 -or $Row -ge $Maze.Height) {
        return $False
    }
    if ($Column -lt 0 -or $Column -ge $Maze.Width) {
        return $False
    }
    return $True
}

$PaperRoll = '@'

function IsPaperRoll()
 {
    param (
        [Maze]$Maze,
        [int]$Row,
        [int]$Column
    )

    if (-not (CheckValidPos -Maze $Maze -Row $Row -Column $Column)) {
        return $False
    }

    return $Maze.Rows[$Row][$Column] -eq $PaperRoll
}

function Get-NeighborCount {
    param (
        [Maze]$Maze,
        [int]$Row,
        [int]$Column
    )

    if ($Row -lt 0 -or $Row -ge $Maze.Height) {
        throw "Invalid row $Row"
    }
    if ($Column -lt 0 -or $Column -ge $Maze.Width) {
        throw "Invalid row $Column"
    }

    $Result = 0

    if (IsPaperRoll -Maze $Maze -Row ($Row - 1) -Column ($Column - 1)) {
        $Result++
    }
    if (IsPaperRoll -Maze $Maze -Row ($Row - 1) -Column $Column) {
        $Result++
    }
    if (IsPaperRoll -Maze $Maze -Row ($Row - 1) -Column ($Column + 1)) {
        $Result++
    }
    if (IsPaperRoll -Maze $Maze -Row $Row -Column ($Column - 1)) {
        $Result++
    }
    if (IsPaperRoll -Maze $Maze -Row $Row -Column ($Column + 1)) {
        $Result++
    }
    if (IsPaperRoll -Maze $Maze -Row ($Row + 1) -Column ($Column - 1)) {
        $Result++
    }
    if (IsPaperRoll -Maze $Maze -Row ($Row + 1) -Column $Column) {
        $Result++
    }
    if (IsPaperRoll -Maze $Maze -Row ($Row + 1) -Column ($Column + 1)) {
        $Result++
    }

    return $Result
}

function Main {
    $ThisMaze = [Maze]::new()
    foreach ($line in Get-Content "input.txt") {
        if ($ThisMaze.Height -eq 0) {
            $ThisMaze.Width = $line.Length
        }
        $ThisMaze.Height += 1
        $ThisMaze.Rows.Add($line) | Out-Null
    }

    # Part 1
    $Answer1 = 0
    for ($i = 0; $i -lt $ThisMaze.Height; $i++) {
        for ($j = 0; $j -lt $ThisMaze.Width; $j++) {
            if ($ThisMaze.Rows[$i][$j] -ne $PaperRoll) {
                continue;
            }
            if ((Get-NeighborCount -Maze $ThisMaze -Row $i -Column $j) -lt 4) {
                $Answer1++
            }
        }
    }

    Write-Host "Question 1: How many rolls of paper can be accessed by a forklift?"
    Write-Host "Answer:", $Answer1
}

Main

# Question 1: How many rolls of paper can be accessed by a forklift?
# Answer: 1320
