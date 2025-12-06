<# 
    Advent of Code 2025
    Day 6: Trash Compactor
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day06.ps1"
#>


class Calculation {
    [int]$Operator = 0 # 0 NOP, 1 +, 2 *
    $Operands = [System.Collections.ArrayList]::new()

    [long] Compute() {
        if($this.Operator -eq 1){
            [long]$Result = 0
            foreach ($Operand in $this.Operands) {
                $Result += $Operand
            }
        } elseif($this.Operator -eq 2) {
            [long]$Result = 1
            foreach ($Operand in $this.Operands) {
                $Result *= $Operand
            }
        } else {
            [long]$Result = -0
            Write-Host "this.Operator $($this.Operator)"
            throw "Invalid Operator!"
        }
        return $Result
    }
}

function Part1([String]$FileName) {
    $Calculations = [System.Collections.ArrayList]::new()
    $FirstLine = $True
    foreach ($Line in Get-Content $FileName) {
        $Operands = $Line -split "\s+"
        $CurrentColumn = 0
        foreach ($Operand in $Operands) {
            # There can be still empty tokens if a line starts or
            # ends with spaces. Some lines of my input ends with 
            # spaces
            if ($Operand -ne "" -and $Operand -ne " ") {
                if ($FirstLine) {
                    $Caluclation = [Calculation]::new()
                    $Caluclation.Operands.Add([long]$Operand) | Out-Null
                    $Calculations.Add($Caluclation) | Out-Null
                } else {
                    if ($Operand -eq "+") {
                        $Calculations[$CurrentColumn].Operator = 1
                    } elseif ($Operand -eq "*") {
                        $Calculations[$CurrentColumn].Operator = 2
                    } else {
                        $Calculations[$CurrentColumn].Operands.Add([long]$Operand) | Out-Null
                    }
                }
                $CurrentColumn++
            }
        }
        $firstLine = $False
    }
    [long]$Result = 0
    foreach ($Calculation in $Calculations) {
        $Result += $Calculation.Compute()
    }
    return $Result
}

function ParseNumber($Lines, $OnesIndex, $Column)
{
    $Digits = [System.Collections.ArrayList]::new()
    for ($r = 0; $r -le $OnesIndex; $r++)
    {
        $d = $Lines[$r][$Column]
        if ($d -ne " ") {
            $Digits.Add($d) | Out-Null
        }
    }
    if ($Digits.Count -eq 0) {
        return $Null
    } else {
        $NumberString = [String]::new($Digits)
        return [long]$NumberString
    }
}

function Part2([String]$FileName) {
    [long]$Result = 0
    $Lines = Get-Content $FileName
    $Width = $Lines[0].Length
    $OnesIndex = $Lines.Count - 2
    $LastLineIndex = $Lines.Count - 1
    
    $Calculation = $Null
    for($i = $Width-1; $i -ge 0; $i--){
        $Number = ParseNumber -Lines $Lines -OnesIndex $OnesIndex -Column $i
        if ($Null -ne $Number) {
            if($Null -eq $Calculation) {
                $Calculation = [Calculation]::new()
            }
        } else {
            continue
        }

        $Calculation.Operands.Add($Number) | Out-Null

        $Operand = $Lines[$LastLineIndex][$i]
        if ($Operand -eq "+") {
            $Calculation.Operator = 1
            $Result += $Calculation.Compute()
            $Calculation = $Null
        }elseif ($Operand -eq "*") {
            $Calculation.Operator = 2
            $Result += $Calculation.Compute()
            $Calculation = $Null
        }
    }

    return $Result
}

function Main {
    [long]$Answer1 = Part1 -FileName "input.txt"
    Write-Host "Question 1: What is the grand total found by adding together all of the answers to the individual problems?"
    Write-Host "Answer:", $Answer1

    [long]$Answer2 = Part2 -FileName "input.txt"
    Write-Host "Question 2: What is the grand total found by adding together all of the answers to the individual problems?"
    Write-Host "Answer:", $Answer2
}

Main

# Question 1: What is the grand total found by adding together all of the answers to the individual problems?
# Answer: 5227286044585
# Question 2: What is the grand total found by adding together all of the answers to the individual problems?
# Answer: 10227753257799
