<# 
    Advent of Code 2025
    Day 12: Christmas Tree Farm
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day12.ps1"
#>

class Present
{
    $Lines
    $TilesCount
}

class Region
{
    [int]$ColumnCount = 0
    [int]$RowCount = 0
    $PresentCounts = [System.Collections.ArrayList]::new()
    $Presents = [System.Collections.ArrayList]::new()
}
$Presents = [System.Collections.ArrayList]::new()
$Regions = [System.Collections.ArrayList]::new()

function ParseInput($Lines) {
    $EmptyLineIndices = [System.Collections.ArrayList]::new()
    for ($i = 0; $i -lt $Lines.Count; $i++) {
        if ($Lines[$i].Length -eq 0) {
            $Null = $EmptyLineIndices.Add($i)
        }
    }

    # Parse Regions
    for ($i = $EmptyLineIndices[-1] + 1; $i -lt $Lines.Count; $i++) {
        $Region = [Region]::new()
        $Tokens = $Lines[$i] -Split " "
        $Dimensions = $Tokens[0] -Split "x"
        $Region.ColumnCount = [int]$Dimensions[0]
        $Region.RowCount = [int]$Dimensions[1].SubString(0, $Dimensions[1].Length - 1)
        for ($j = 1; $j -lt $Tokens.Count; $j++){
            $PresentIndex = $j - 1
            $Count = [int]$Tokens[$j]
            $Null = $Region.PresentCounts.Add($Count)
            for($k = 0; $k -lt $Count; $k++) {
                $Null = $Region.Presents.Add($PresentIndex)
            }
        }
        $Null = $Regions.Add($Region)
    }

    # Parse Presents
    $EmptyLineIndices.Insert(0, -1)
    for ($i = 0; ($i + 1) -lt $EmptyLineIndices.Count; $i++) {
        $Present = [Present]::new()
        $Present.Lines = $Lines[($EmptyLineIndices[$i] + 2)..($EmptyLineIndices[$i+1] - 1)]
        $Present.TilesCount = ([regex]::Matches($Present.Lines, "#")).Count
        $Null = $Presents.Add($Present)
    }
}

function Main {
    $Lines = Get-Content "input.txt"
    ParseInput($Lines)
    
    $Answer1 = 0
    Write-Host "Question 1: How many of the regions can fit all of the presents listed?"
    foreach ($Region in $Regions) {
        # Actually it's just by accident that the answer is correct since I'm not really
        # packing the presents, but just comparing region size with the total size of all presents.
        $RegionSize = $Region.ColumnCount * $Region.RowCount
        $TotalPresentSize = 0
        for ($i = 0; $i -lt $Region.PresentCounts.Count; $i++ ) {
            $TotalPresentSize += $Region.PresentCounts[$i] * $Presents[$i].TilesCount
        }
        # Write-Host $RegionSize, $TotalPresentSize
        if ($RegionSize -ge $TotalPresentSize) {
            $Answer1 += 1
        }
    }
    Write-Host "Answer:", $Answer1
}

Main

# Question 1: How many of the regions can fit all of the presents listed?
# Answer: 451
