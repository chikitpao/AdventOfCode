<# 
    Advent of Code 2025
    Day 3: Lobby
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day03.ps1"
#>

function Get-Joltage1($ListParam) {
    $MaxValue = 0
    for($i = 0; $i -lt $ListParam.Length; $i++) {
        $d1 = $ListParam[$i]
        $temp1 = $d1 * 10
        for($j = $i + 1; $j -lt $ListParam.Length; $j++) {
            $d2 = $ListParam[$j]
            $temp2 = $temp1 + $d2
            if ($temp2 -gt $MaxValue) {
                $MaxValue = $temp2
            }
        }
    }
    return $MaxValue
}

# Hashtable for Memoization
[hashtable] $CachedResults = @{}

function Get-Joltage2([String]$StringParam, [int]$Remaining) {
    if ($StringParam.Length -lt $Remaining) {
        throw "List too short!"
    }
    if ($Remaining -eq 0) {
        throw "Remaining is 0!"
    }

    $Key = ('{0:d4}' -f $Remaining) + ":" + $StringParam
    if ($CachedResults.Contains($Key)) {
        return $CachedResults[$Key]
    }

    [long]$CurrentPlaceValue = [Math]::Pow(10, $Remaining - 1) * [int]$StringParam.Substring(0, 1)

    if ($StringParam.Length -eq $Remaining) {
        # Take all digits since we cannot skip digit anymore.
        if ($Remaining -eq 1) {
            $Result = $CurrentPlaceValue
            $CachedResults[$Key] = $Result
            return $Result
        } else {
            $Result = $CurrentPlaceValue + (Get-Joltage2 -StringParam $StringParam.Substring(1) -Remaining ($Remaining - 1))
            $CachedResults[$Key] = $Result
            return $Result
        }
    }

    # Skip first digit
    [long]$v1 = Get-Joltage2 -StringParam $StringParam.Substring(1) -Remaining $Remaining
    # Use first digit
    [long]$v2 = $CurrentPlaceValue
    if ($Remaining -gt 1) {
        $v2 += Get-Joltage2 -StringParam $StringParam.Substring(1) -Remaining ($Remaining - 1)
    }

    if ($v1 -gt $v2) {
        $Result = $v1
    } else {
        $Result = $v2
    }
    
    $CachedResults[$Key] = $Result
    return $Result
}


function Main {
   $Answer1 = 0
   [long]$Answer2 = 0

    foreach ($line in Get-Content "input.txt") {

        # Part 1
        [System.Collections.ArrayList]$list = @()
        foreach ($c in $line.ToCharArray(0, $line.Length)) {
            [array]$list += [int]"$c"
        }
        $Answer1 += Get-Joltage1 -ListParam $list
        
        # Part2
        $Answer2 += Get-Joltage2 -StringParam $line -Remaining 12
        Write-Host $Answer2
    }

    Write-Host "Question 1: What is the total output joltage?"
    Write-Host "Answer:", $Answer1
    Write-Host "Question 2: What is the new total output joltage?"
    Write-Host "Answer:", $Answer2
}

Main

# Question 1: What is the total output joltage?
# Answer: 16842 
# Question 2: What is the new total output joltage?
# Answer: 167523425665348
