<# 
    Advent of Code 2025
    Day 9: Movie Theater
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day09.ps1"
#>

[void] [System.Reflection.Assembly]::LoadWithPartialName("System.Drawing") 
[void] [System.Reflection.Assembly]::LoadWithPartialName("System.Windows.Forms")

class Tile{
    [int]$Id
    [int]$Row
    [int]$Column
}

class Pair{
    [int]$Id1 = 0
    [int]$Id2 = 0
    [long]$Area = 0
    [int]$From  = 0 # Start Column / Row for Horizonal or Vertical Strips
    [int]$To  = 0 # End Column / Row for Horizonal or Vertical Strips
}

$MinColumn = $Null
$MaxColumn = $Null
$MinRow = $Null
$MaxRow = $Null
$SortedRectangles = [System.Collections.ArrayList]::new()
$HorizontalEdges = [System.Collections.ArrayList]::new()
$VerticalEdges = [System.Collections.ArrayList]::new()

function ReadInput($FileName) {
    $Tiles = [System.Collections.ArrayList]::new()
    foreach ($Line in Get-Content $FileName) {
        $Coordinates = $Line -split "[,]"
        $Tile = [Tile]::new()
        $Tile.Id = $CurrentId
        $Tile.Column = [int]$Coordinates[0]
        $Tile.Row = [int]$Coordinates[1]
        $Tiles.Add($Tile) | Out-Null
        $CurrentId++
        if (($Null -eq $global:MinColumn) -or ($global:MinColumn -gt $Tile.Column)) {
            $global:MinColumn = $Tile.Column
        }
        if (($Null -eq $global:MaxColumn) -or ($global:MaxColumn -lt $Tile.Column)) {
            $global:MaxColumn = $Tile.Column
        }
        if (($Null -eq $global:MinRow) -or ($global:MinRow -gt $Tile.Row)) {
            $global:MinRow = $Tile.Row
        }
        if (($Null -eq $global:MaxRow) -or ($global:MaxRow -lt $Tile.Row)) {
            $global:MaxRow = $Tile.Row
        }
    }

    $Rectangles = [System.Collections.ArrayList]::new()
    for($i = 0; $i -lt $Tiles.Count; $i++) {
        for($j = $i + 1; $j -lt $Tiles.Count; $j++) {
            $Area = ([Math]::Abs($Tiles[$j].Column - $Tiles[$i].Column) + 1) * ([Math]::Abs($Tiles[$j].Row - $Tiles[$i].Row) + 1)
            $Pair = [Pair]::new()
            $Pair.Area = $Area
            if ($Tiles[$i].Row -eq $Tiles[$j].Row) {
                if($Tiles[$i].Column -lt $Tiles[$j].Column) {
                    $Pair.Id1 = $i
                    $Pair.Id2 = $j
                    $Pair.From = $Tiles[$i].Column
                    $Pair.To = $Tiles[$j].Column
                } else {
                    $Pair.Id2 = $i
                    $Pair.Id1 = $j
                    $Pair.To = $Tiles[$i].Column
                    $Pair.From = $Tiles[$j].Column
                }
                $Null = $Rectangles.Add($Pair)
                if (($i + 1 -eq $j) -or (($i -eq 0) -and ($j -eq ($Tiles.Count -1)))) {
                    $Null = $global:HorizontalEdges.Add($Pair)
                }
            } elseif ($Tiles[$i].Column -eq $Tiles[$j].Column) {
                if($Tiles[$i].Row -lt $Tiles[$j].Row) {
                    $Pair.Id1 = $i
                    $Pair.Id2 = $j
                    $Pair.From = $Tiles[$i].Row
                    $Pair.To = $Tiles[$j].Row
                } else {
                    $Pair.Id2 = $i
                    $Pair.Id1 = $j
                    $Pair.To = $Tiles[$i].Row
                    $Pair.From = $Tiles[$j].Row
                }
                $Null = $Rectangles.Add($Pair)
                if ((($i + 1) -eq $j) -or (($i -eq 0) -and ($j -eq ($Tiles.Count -1)))) {
                    $Null = $global:VerticalEdges.Add($Pair)
                }
            } else {
                $Pair.Id1 = $i
                $Pair.Id2 = $j
                $Null = $Rectangles.Add($Pair)
            }
        }
    }

    $global:SortedRectangles = $Rectangles | Sort-Object -Property Area -Descending

    return $Tiles
}

function ShowTiles($Tiles, $Rectangle) {
    $objForm = New-Object System.Windows.Forms.Form
    $objForm.StartPosition = "CenterScreen"
    $objForm.Text = "Advent of Code 2025, Day 9, Part 2"
    $Cx = 700
    $Cy = 700 
    $objForm.Size = New-Object System.Drawing.Size($Cx, $Cy)
    
    $BlackPen = new-object Drawing.Pen black
    $FormGraphics = $objForm.createGraphics()
    $objForm.add_paint(
    {
        $XScale = ($Cx - 90) / $MaxColumn
        $YScale = ($Cy - 90) / $MaxRow
        foreach ($s in $global:HorizontalEdges) {
            $FormGraphics.DrawLine($BlackPen, $Tiles[$s.Id1].Column * $XScale, $Tiles[$s.Id1].Row * $YScale, $Tiles[$s.Id2].Column * $XScale, $Tiles[$s.Id2].Row * $YScale)
        }
        foreach ($s in $global:VerticalEdges) {
            $FormGraphics.DrawLine($BlackPen, $Tiles[$s.Id1].Column * $XScale, $Tiles[$s.Id1].Row * $YScale, $Tiles[$s.Id2].Column * $XScale, $Tiles[$s.Id2].Row * $YScale)
        }

        $RedPen = new-object Drawing.Pen red
        $FormGraphics.DrawLine($RedPen, $Tiles[$Rectangle.Id1].Column * $XScale, $Tiles[$Rectangle.Id1].Row * $YScale, $Tiles[$Rectangle.Id2].Column * $XScale, $Tiles[$Rectangle.Id1].Row * $YScale)
        $FormGraphics.DrawLine($RedPen, $Tiles[$Rectangle.Id2].Column * $XScale, $Tiles[$Rectangle.Id1].Row * $YScale, $Tiles[$Rectangle.Id2].Column * $XScale, $Tiles[$Rectangle.Id2].Row * $YScale)
        $FormGraphics.DrawLine($RedPen, $Tiles[$Rectangle.Id1].Column * $XScale, $Tiles[$Rectangle.Id2].Row * $YScale, $Tiles[$Rectangle.Id2].Column * $XScale, $Tiles[$Rectangle.Id2].Row * $YScale)
        $FormGraphics.DrawLine($RedPen, $Tiles[$Rectangle.Id1].Column * $XScale, $Tiles[$Rectangle.Id1].Row * $YScale, $Tiles[$Rectangle.Id1].Column * $XScale, $Tiles[$Rectangle.Id2].Row * $YScale)
    }
    )
    [void] $objForm.ShowDialog()
}

function HasIntersection($Rectangle) {
    foreach ($Edge in $HorizontalEdges) {
        # Edge intersects with Rectangle?
        $Difference1 = $Tiles[$Rectangle.Id1].Row - $Tiles[$Edge.Id1].Row
        $Difference2 = $Tiles[$Rectangle.Id2].Row - $Tiles[$Edge.Id1].Row
        if ([Math]::Sign($Difference1) -eq [Math]::Sign($Difference2)) {
            continue
        }
        if (($Difference1 -eq 0) -or ($Difference2 -eq 0)) {
            continue
        }
        if (($Tiles[$Rectangle.Id1].Column -gt $Edge.From) -and ($Tiles[$Rectangle.Id1].Column -lt $Edge.To)) {
            return $True
        }
        if (($Tiles[$Rectangle.Id2].Column -gt $Edge.From) -and ($Tiles[$Rectangle.Id2].Column -lt $Edge.To)) {
            return $True
        }

        # Edge inside Rectangle?
        if ($Tiles[$Rectangle.Id1].Column -lt $Tiles[$Rectangle.Id2].Column) {
            $RectangleMin = $Tiles[$Rectangle.Id1].Column
            $RectangleMax = $Tiles[$Rectangle.Id2].Column
        } else {
            $RectangleMin = $Tiles[$Rectangle.Id2].Column
            $RectangleMax = $Tiles[$Rectangle.Id1].Column
        }
        if (($Edge.From -gt $RectangleMin) -and ($Edge.From -lt $RectangleMax)) {
            return $True
        }
        if (($Edge.To -gt $RectangleMin) -and ($Edge.To -lt $RectangleMax)) {
            return $True
        }
    }
    foreach ($Edge in $VerticalEdges) {
        # Edge intersects with Rectangle?
        $Difference1 = $Tiles[$Rectangle.Id1].Column - $Tiles[$Edge.Id1].Column
        $Difference2 = $Tiles[$Rectangle.Id2].Column - $Tiles[$Edge.Id1].Column
        if ([Math]::Sign($Difference1) -eq [Math]::Sign($Difference2)) {
            continue
        }
        if (($Difference1 -eq 0) -or ($Difference2 -eq 0)) {
            continue
        }
        if (($Tiles[$Rectangle.Id1].Row -gt $Edge.From) -and ($Tiles[$Rectangle.Id1].Row -lt $Edge.To)) {
            return $True
        }
        if (($Tiles[$Rectangle.Id2].Row -gt $Edge.From) -and ($Tiles[$Rectangle.Id2].Row -lt $Edge.To)) {
            return $True
        }

        # Edge inside Rectangle?
        if ($Tiles[$Rectangle.Id1].Row -lt $Tiles[$Rectangle.Id2].Row) {
            $RectangleMin = $Tiles[$Rectangle.Id1].Row
            $RectangleMax = $Tiles[$Rectangle.Id2].Row
        } else {
            $RectangleMin = $Tiles[$Rectangle.Id2].Row
            $RectangleMax = $Tiles[$Rectangle.Id1].Row
        }
        if (($Edge.From -gt $RectangleMin) -and ($Edge.From -lt $RectangleMax)) {
            return $True
        }
        if (($Edge.To -gt $RectangleMin) -and ($Edge.To -lt $RectangleMax)) {
            return $True
        }
    }
    return $False
}

function Part2($Tiles) {
    foreach($Rectangle in $SortedRectangles) {
        # There should be no edges cut through the interior of the rectangle or inside the rectangle.
        if (HasIntersection($Rectangle)) {
            continue
        }
        return $Rectangle
    }
    return $Null
}
function Main {
    $Tiles = ReadInput("input.txt")

    Write-Host "Question 1: Using two red tiles as opposite corners, what is the largest area of any rectangle you can make?"
    Write-Host "Answer:", $SortedRectangles[0].Area

    Write-Host "Question 2: Using two red tiles as opposite corners, what is the largest area of any rectangle you can make using only red and green tiles?"
    Write-Host "HorizontalEdges.Count", $HorizontalEdges.Count
    Write-Host "VerticalEdges.Count", $VerticalEdges.Count
    Write-Host "Rectangles.Count", $SortedRectangles.Count
    $Rectangle = Part2 -Tiles $Tiles
    Write-Host "Answer:", $Rectangle.Area

    ShowTiles -Tiles $Tiles -HorizontalStripes $HorizontalEdges -VerticalStripes $VerticalEdges -Rectangle $Rectangle
}

Main

# Question 1: Using two red tiles as opposite corners, what is the largest area of any rectangle you can make?
# Answer: 4735268538
# Question 2: Using two red tiles as opposite corners, what is the largest area of any rectangle you can make using only red and green tiles?"
# Answer: 1537458069
