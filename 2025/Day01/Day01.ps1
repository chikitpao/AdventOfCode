<# 
    Advent of Code 2025
    Day 1: Secret Entrance
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day01.ps1"
#>

function Main {
    $TotalSteps = 100
    $ZeroCount1 = 0
    $ZeroCount2 = 0
    $CurrentPos = 50

    foreach ($line in Get-Content "input.txt") {
        if ($line -match "(?<dir>[LR])(?<steps>\d+)") {
            
            $Steps = [int]$Matches["steps"]
            $a = [int] [Math]::Truncate($Steps / $TotalSteps)
            $r = $Steps % $TotalSteps
            if ($Steps -eq 0 -or $r -eq 0) {
                Write-Host $Steps "congruent to 0 mod" $TotalSteps
            }
            $ZeroCount2 += $a
            $Steps %= $TotalSteps
            if ($Matches["dir"] -eq "L") {
                $Steps = -$Steps
            }
            $OldPos = $CurrentPos
            $CurrentPos += $Steps
            $CurrentPos %= $TotalSteps
            if ($CurrentPos -lt 0) {
                $CurrentPos += $TotalSteps
            }

            if ($OldPos -eq 0 -and $CurrentPos -eq 0) {
                Write-Host "0 to 0"
            }

            if ($CurrentPos -eq 0) {
                $ZeroCount1++
                $ZeroCount2++
            } elseif ($OldPos -ne 0) {
                if ($Matches["dir"] -eq "L" -and $OldPos -lt $CurrentPos) {
                    $ZeroCount2++
                } elseif ($Matches["dir"] -eq "R" -and $OldPos -gt $CurrentPos) {
                    $ZeroCount2++
                }
            }
        }
    }
    
    Write-Host "Question 1: What's the actual password to open the door?"
    Write-Host "Answer:", $ZeroCount1
    Write-Host "Question 2: Using password method 0x434C49434B, what is the password to open the door?"
    Write-Host "Answer:", $ZeroCount2
}

Main

# Question 1: What's the actual password to open the door?
# Answer: 1725
# Question 2: Using password method 0x434C49434B, what is the password to open the door?"
# Answer: 6175
