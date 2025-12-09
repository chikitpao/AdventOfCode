<# 
    Advent of Code 2025
    Day 9: Movie Theater
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day09.ps1"
#>

class Tile{
    [int]$Id
    [int]$Row
    [int]$Column
}

function Main {
    $Tiles = [System.Collections.ArrayList]::new()
    foreach ($Line in Get-Content "input.txt") {
        $Coordinates = $Line -split "[,]"
        $Tile = [Tile]::new()
        $Tile.Id = $CurrentId
        $Tile.Column = [int]$Coordinates[0]
        $Tile.Row = [int]$Coordinates[1]
        $Tiles.Add($Tile) | Out-Null
        $CurrentId++
    }

    $Answer1 = 0
    for($i = 0; $i -lt $Tiles.Count; $i++) {
        for($j = $i + 1; $j -lt $Tiles.Count; $j++) {
            $Area = ([Math]::Abs($Tiles[$j].Column - $Tiles[$i].Column) + 1) * ([Math]::Abs($Tiles[$j].Row - $Tiles[$i].Row) + 1)
            if($Area -gt $Answer1) {
                $Answer1 = $Area
            }
        }
    }

    Write-Host "Question 1: Using two red tiles as opposite corners, what is the largest area of any rectangle you can make?"
    Write-Host "Answer:", $Answer1
}

Main

# Question 1: Using two red tiles as opposite corners, what is the largest area of any rectangle you can make?
# Answer: 4735268538
