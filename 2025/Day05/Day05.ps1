<# 
    Advent of Code 2025
    Day 5: Cafeteria
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day05.ps1"
#>


class Range {
    [long]$Lower = 0
    [long]$Upper = 0
}

function CloneRange ([Range]$Range) {
    $NewRange = [Range]::new()
    $NewRange.Lower = $Range.Lower
    $NewRange.Upper = $Range.Upper
    return $NewRange
}

function MergeOverlap([array]$Ranges) {
    # Algorithm for Merging Overlapping Intervals from
    # https://www.geeksforgeeks.org/dsa/merging-intervals/

     # Sort intervals based on start values
    $SortedRanges = $Ranges | Sort-Object -Property Lower

    $NewRanges = [System.Collections.ArrayList]::new()

    # REMARK: Without Out-Null after Add (or assign result of Add to $null), 
    # the ArrayList returned by this function will have a lot of int 1 preceding the 
    # actual result!
    # $NewRanges.Add((CloneRange -Range $SortedRanges[0])) | Out-Null
    $null = $NewRanges.Add((CloneRange -Range $SortedRanges[0]))
    
    for ($i = 1; $i -lt $SortedRanges.Count; $i++) {
        $Last = $NewRanges[-1]
        $Current = $SortedRanges[$i]

        # If current interval overlaps with the last merged
        # interval, merge them 
        if ($Current.Lower -le $Last.Upper) {
            if($Last.Upper -lt $Current.Upper) {
                $Last.Upper = $Current.Upper
            }
        } else {
            # $null = $NewRanges.Add((CloneRange -Range $Current))
            $NewRanges.Add((CloneRange -Range $Current)) | Out-Null
        }
    }

    $SortedRanges2 = $NewRanges | Sort-Object -Property Lower 
    return $SortedRanges2
}


function Main {
    # Part 1
    [long]$Answer1 = 0
    $parseId = $False
    foreach ($Line in Get-Content "input.txt") {
        if (-not $parseId -and $Line.Length -eq 0) {
            $parseId = $True
            continue
        }

        if (-not $parseId) {
            $Boundaries = $Line -split "[-]"
            $NewRange = [Range]::new()
            $NewRange.Lower = [long]$Boundaries[0]
            $NewRange.Upper = [long]$Boundaries[1]
            [array]$Ranges += $NewRange
        } else {
            $Id = [long]$Line
            foreach ($p in $Ranges) {
                if ($id -gt $p.Lower -and $id -lt $p.Upper) {
                    $Answer1++
                    break
                }
            }
        }
    }
    Write-Host "Question 1: How many of the available ingredient IDs are fresh?"
    Write-Host "Answer:", $Answer1

    # Part2
    [long]$Answer2 = 0
    $NewRanges = MergeOverlap -Ranges $Ranges
    foreach ($Range in $NewRanges) {
        # Write-Host ($Range  | Format-Table | Out-String)
        # Write-Host ($Range  | Format-List | Out-String)
        $Answer2 += ($Range.Upper - $Range.Lower + 1)
    }
    Write-Host "Question 2: How many ingredient IDs are considered to be fresh according to the fresh ingredient ID ranges?"
    Write-Host "Answer:", $Answer2
}

Main

# Question 1: How many of the available ingredient IDs are fresh?
# Answer: 520
# Question 2: How many ingredient IDs are considered to be fresh according to the fresh ingredient ID ranges?
# Answer: 347338785050515
