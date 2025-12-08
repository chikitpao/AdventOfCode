<# 
    Advent of Code 2025
    Day 8: Playground
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day08.ps1"
#>

class Box{
    [int]$Id
    [int]$X
    [int]$Y
    [int]$Z
}

class Connection{
    [double]$Distance
    [int]$From
    [int]$To
}

function GetDistance([Box]$Box1, [Box]$Box2) {
    return [Math]::sqrt([Math]::Pow($Box2.X - $Box1.X, 2) + [Math]::Pow($Box2.Y - $Box1.Y, 2) + [Math]::Pow($Box2.Z - $Box1.Z, 2))
}

function GetIndexInConnected($ConnectedBoxes, $UnconnectedBoxes, $BoxId) {
    if($UnconnectedBoxes.Contains($BoxId)) {
        $UnconnectedBoxes.Remove($BoxId) | Out-Null
        $ConnectedBoxes.Add([System.Collections.ArrayList]::new()) | Out-Null
        $ConnectedBoxes[-1].Add($BoxId) | Out-Null
        return $ConnectedBoxes.Count - 1
    } else {
        for($i = 0; $i -lt $ConnectedBoxes.Count; $i++){
            if ($ConnectedBoxes[$i].Contains($BoxId)) {
                return $i
            }
        }
        throw "Box ID not found!"
    }
}

function MergeConnections($ConnectedBoxes, $From, $To){
    if ($From -eq $To) {
        return
    }
    
    if ($From -gt $To) {
        $Temp = $To
        $To = $From
        $From = $Temp
    }
    foreach ($Id in $ConnectedBoxes[$To]) {
        $ConnectedBoxes[$From].Add($Id) | Out-Null
    }
    $ConnectedBoxes[$To] = $Null
    if($To -le ($ConnectedBoxes.Count - 1)) {
        $ConnectedBoxes[$To] = $ConnectedBoxes[-1]
    }
    $ConnectedBoxes.Remove(($ConnectedBoxes[-1]))
}

function Part1($Boxes, $SortedConnections, $NeededConnectionCount, $NeededResultCount){
    $ConnectedBoxes = [System.Collections.ArrayList]::new()
    $UnconnectedBoxes = New-Object System.Collections.Generic.HashSet[int]
    for($i = 0; $i -lt $Boxes.Count; $i++) {
        $UnconnectedBoxes.Add($i) | Out-Null
    }
    for($i = 0; $i -lt $NeededConnectionCount; $i++){
        $Connection = $SortedConnections[$i]
        $FromIndexInConnected = (GetIndexInConnected -ConnectedBoxes $ConnectedBoxes -UnconnectedBoxes $UnconnectedBoxes -BoxId $Connection.From)
        $ToIndexInConnected = (GetIndexInConnected -ConnectedBoxes $ConnectedBoxes -UnconnectedBoxes $UnconnectedBoxes -BoxId $Connection.To)
        MergeConnections -ConnectedBoxes $ConnectedBoxes -From $FromIndexInConnected -To $ToIndexInConnected
    }

    $BoxesCount = [System.Collections.ArrayList]::new()
    foreach ($ConnectedBox in $ConnectedBoxes) {
        $BoxesCount.Add($ConnectedBox.Count) | Out-Null
    }
    foreach ($UnconnectedBox in $UnconnectedBoxes) {
        $BoxesCount.Add(1) | Out-Null
    }
    $SortedBoxesCount = $BoxesCount | Sort-Object -Descending
    $Result = 1
    for($i = 0; $i -lt $NeededResultCount; $i++) {
        $Result *= $SortedBoxesCount[$i]
    }
    return $Result
}

function Main {
    $CurrentId = 0
    $Boxes = [System.Collections.ArrayList]::new()
    foreach ($Line in Get-Content "input.txt") {
        $Coordinates = $Line -split "[,]"
        $Box = [Box]::new()
        $Box.Id = $CurrentId
        $Box.X = [int]$Coordinates[0]
        $Box.Y = [int]$Coordinates[1]
        $Box.Z = [int]$Coordinates[2]
        $Boxes.Add($Box) | Out-Null
        $CurrentId++
    }

    $Connections = [System.Collections.ArrayList]::new()
    for ($i = 0; $i -lt $Boxes.Count; $i++){
        for ($j = $i + 1; $j -lt $Boxes.Count; $j++) {
            $Connection = [Connection]::new()
            $Connection.Distance = (GetDistance -Box1 $Boxes[$i] -Box2 $Boxes[$j])
            $Connection.From = $i
            $Connection.To = $j
            $Connections.Add($Connection) | Out-Null
        }
    }
    $SortedConnections = $Connections | Sort-Object -Property Distance

    Write-Host "Question 1: Connect together the 1000 pairs of junction boxes which are closest together. Afterward, what do you get if you multiply together the sizes of the three largest circuits?"
    Write-Host "Answer:", (Part1 -Boxes $Boxes -SortedConnections $SortedConnections -NeededConnectionCount 1000 -NeededResultCount 3)
}

Main

# Question 1: Connect together the 1000 pairs of junction boxes which are closest together. Afterward, what do you get if you multiply together the sizes of the three largest circuits?
# Answer: 57564
