<# 
    Advent of Code 2025
    Day 7: Laboratories
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day07.ps1"
#>

class Pos{
    [int]$Row
    [int]$Column
}

class Quiz{
    [int]$RowCount
    [int]$ColumnCount
    [Pos]$Start
    $Lines
}

$Splitter = "^"

function InitQuiz($Lines){
    # Even Row Index: Start 'S' or splitters '^ 
    # Odd Row Index: Empty '.'
    $Quiz = [Quiz]::new()
    $Quiz.RowCount = $Lines.Count
    $Quiz.ColumnCount = $Lines[0].Length
    $Quiz.Lines = $Lines

    $Quiz.Start = [Pos]::new()
    $Quiz.Start.Row = 0
    $Quiz.Start.Column = $Quiz.Lines[0].IndexOf("S")
    if ($Quiz.Start.Column % 2 -ne 0) {
        throw "Column Index of Start Position is not even!"
    }
    for ($Row = 2; $Row -lt $Quiz.Lines.Count; $Row+=2) {
        for($Column = 0; $Column -lt $Quiz.Lines[$Row].Length; $Column++) {
            if ($Quiz.Lines[$Row][$Column] -eq $Splitter) {
                if ($Column % 2 -eq ([int] [Math]::Truncate($Row / 2)) % 2) {
                    throw "Something is wrong about Logic for Splitter Column Index!"
                }
            }
        }
    }
    return $Quiz
}

function Get-EmptyBeamList($ColumnCount)
{
    $Result = [System.Collections.ArrayList]::new()
    for($Column = 0; $Column -lt $ColumnCount; $Column++) {
        $Result.Add($False) | Out-Null
    }
    return $Result
}

function Part1($Quiz) {
    $Splits = 0
    $OldBeams = Get-EmptyBeamList($Quiz.ColumnCount)
    $NewBeams = Get-EmptyBeamList($Quiz.ColumnCount)

    $OldBeams[$Quiz.Start.Column] = $True
    for ($Row = 2; $Row -lt $Quiz.RowCount; $Row+=2) {
        for ($Column = 0; $Column -lt $Quiz.ColumnCount; $Column++) {
            if ($Quiz.Lines[$Row][$Column] -eq $Splitter) {
                if($OldBeams[$Column]) {
                    $Splits += 1
                    $NewBeams[$Column - 1] = $True
                    $NewBeams[$Column + 1] = $True
                }
            } else {
                if($OldBeams[$Column]) {
                    $NewBeams[$Column] = $True
                }
            }
        }
        $OldBeams = $NewBeams
        $NewBeams = Get-EmptyBeamList($Quiz.ColumnCount)
    }
    return $Splits
}

function Main {
    $Lines = Get-Content "input.txt"
    
    $Quiz = InitQuiz($Lines)

    Write-Host "Question 1: How many times will the beam be split?"
    Write-Host "Answer:", (Part1 $Quiz)
}

Main

# Question 1: How many times will the beam be split?
# Answer: 1570
