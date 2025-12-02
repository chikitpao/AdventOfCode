<# 
    Advent of Code 2025
    Day 2: Gift Shop
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day02.ps1"
#>


function AddInvalidIds ($Low, $High) {
    [long]$Answer = 0
    [long]$i = 1
    for (; $i -le $High; $i++) {
        # REMARK: Difficult to get 10^n in long.
        $TestVar = [long]([String] $i + [String] $i)
        if ($TestVar -gt $High) {
            break
        }
        if ($TestVar -ge $Low) {
            $Answer += $TestVar
        }
    }
    return $Answer
}

function Main {
    $Line = Get-Content "input.txt" -First 1
    $Ranges = $Line -split "[,]"
    [long]$Answer1 = 0
    foreach ($Range in $Ranges) {
        $Boundaries = $Range -split "[-]"
        # REMARK: int was too small
        $L = [long]$Boundaries[0]
        $R = [long]$Boundaries[1]
        $Answer1 += AddInvalidIds -Low $L -High $R
    }
    
    Write-Host "Question 1: What do you get if you add up all of the invalid IDs?"
    Write-Host "Answer:", $Answer1
}

Main

# Question 1: What do you get if you add up all of the invalid IDs?
# Answer: 31000881061

