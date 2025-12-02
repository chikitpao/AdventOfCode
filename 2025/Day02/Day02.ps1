<# 
    Advent of Code 2025
    Day 2: Gift Shop
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day02.ps1"
#>

function AddInvalidIds1 ($Low, $High) {
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
    [long]$MaxValue = 0
    $Line = Get-Content "input.txt" -First 1
    $Ranges = $Line -split "[,]"
    
    # Part 1
    [long]$Answer1 = 0
    foreach ($Range in $Ranges) {
        $Boundaries = $Range -split "[-]"
        # REMARK: int was too small
        $L = [long]$Boundaries[0]
        $R = [long]$Boundaries[1]
        if ($R -gt $MaxValue) {
            $MaxValue = $R
        }
        [array]$Pairs += New-Object psobject -Property @{
            lower  = $L
            upper = $R
        }
        $Answer1 += AddInvalidIds1 -Low $L -High $R
    }

    # Part2
    [long]$Answer2 = 0
    $finished = $False
    $Set = New-Object System.Collections.Generic.HashSet[long]
    for ([long]$i = 1; $i -le $MaxValue; $i++) {
        $str = $i.toString()
        $tempStr = $str
        for ($j = 2; ($j * $str.Length) -le [Math]::Ceiling([Math]::Log10($MaxValue)); $j++) {
            $tempStr += $str
            $tempValue = [long]$tempStr
            if ($tempValue -gt $MaxValue){
                if ($j -eq 2) {
                    $finished = $True
                    break
                } else {
                    continue
                }
            }
            # REMARK: Need to eliminate duplicates (e.g. "2" * 4 and "22" * 2).
            $Set.Add($tempValue) | Out-Null
        }
        if ($finished) {
            break
        }
    }
    foreach ($v in $Set) {
        foreach ($p in $Pairs) {
            if($p.lower -le $v -and $p.upper -ge $v) {
                $Answer2 += $v
            }
        }
    }
    
    Write-Host "Question 1: What do you get if you add up all of the invalid IDs?"
    Write-Host "Answer:", $Answer1
    Write-Host "Question 2: What do you get if you add up all of the invalid IDs using these new rules?"
    Write-Host "Answer:", $Answer2
}

Main

# Question 1: What do you get if you add up all of the invalid IDs?
# Answer: 31000881061
# Question 2: What do you get if you add up all of the invalid IDs using these new rules?
# Answer: 46769308485
