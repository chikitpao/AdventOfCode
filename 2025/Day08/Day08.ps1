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
    [int]$Group
}

class Connection{
    [double]$Distance
    [int]$From
    [int]$To
}

function GetDistance([Box]$Box1, [Box]$Box2) {
    return [Math]::sqrt([Math]::Pow($Box2.X - $Box1.X, 2) + [Math]::Pow($Box2.Y - $Box1.Y, 2) + [Math]::Pow($Box2.Z - $Box1.Z, 2))
}

function GetIndexInConnected($Boxes, $ConnectedBoxes, $UnconnectedBoxes, $BoxId) {
    if($Boxes[$BoxId].Group -eq -1) {
        $UnconnectedBoxes.Remove($BoxId) | Out-Null
        $ConnectedBoxes.Add([System.Collections.ArrayList]::new()) | Out-Null
        $Boxes[$BoxId].Group = $ConnectedBoxes.Count - 1
        $ConnectedBoxes[-1].Add($BoxId) | Out-Null
        return $ConnectedBoxes.Count - 1
    } else {
        return $Boxes[$BoxId].Group
    }
}

function MergeConnections($Boxes, $ConnectedBoxes, $From, $To){
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
        $Boxes[$Id].Group = $From
    }
    $ConnectedBoxes[$To] = $Null
    if($To -lt ($ConnectedBoxes.Count - 1)) {
        $ConnectedBoxes[$To] = $ConnectedBoxes[-1]
        foreach ($BoxId in $ConnectedBoxes[$To]) {
            $Boxes[$BoxId].Group = $To
        }
    }
    $ConnectedBoxes.RemoveAt($ConnectedBoxes.Count - 1) | Out-Null
}

function Part1($Boxes, $SortedConnections, $NeededConnectionCount, $NeededResultCount){
    $ConnectedBoxes = [System.Collections.ArrayList]::new()
    $UnconnectedBoxes = New-Object System.Collections.Generic.HashSet[int]
    for($i = 0; $i -lt $Boxes.Count; $i++) {
        $UnconnectedBoxes.Add($i) | Out-Null
    }
    for($i = 0; $i -lt $NeededConnectionCount; $i++) {
        $Connection = $SortedConnections[$i]
        $FromIndexInConnected = (GetIndexInConnected -Boxes $Boxes -ConnectedBoxes $ConnectedBoxes -UnconnectedBoxes $UnconnectedBoxes -BoxId $Connection.From)
        $ToIndexInConnected = (GetIndexInConnected -Boxes $Boxes -ConnectedBoxes $ConnectedBoxes -UnconnectedBoxes $UnconnectedBoxes -BoxId $Connection.To)
        MergeConnections -Boxes $Boxes -ConnectedBoxes $ConnectedBoxes -From $FromIndexInConnected -To $ToIndexInConnected
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

function Part2($Boxes, $SortedConnections){
    $ConnectedBoxes = [System.Collections.ArrayList]::new()
    $UnconnectedBoxes = New-Object System.Collections.Generic.HashSet[int]
    for($i = 0; $i -lt $Boxes.Count; $i++) {
        $UnconnectedBoxes.Add($i) | Out-Null
    }

    $i = 0
    do{
        $Connection = $SortedConnections[$i]
        $FromIndexInConnected = (GetIndexInConnected -Boxes $Boxes -ConnectedBoxes $ConnectedBoxes -UnconnectedBoxes $UnconnectedBoxes -BoxId $Connection.From)
        $ToIndexInConnected = (GetIndexInConnected -Boxes $Boxes -ConnectedBoxes $ConnectedBoxes -UnconnectedBoxes $UnconnectedBoxes -BoxId $Connection.To)
        MergeConnections -Boxes $Boxes -ConnectedBoxes $ConnectedBoxes -From $FromIndexInConnected -To $ToIndexInConnected
        if($ConnectedBoxes.Count -eq 1 -and $UnconnectedBoxes.Count -eq 0) {
            return $Boxes[$Connection.From].X * $Boxes[$Connection.To].X
        }
        $i++
    } while ($True)
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
        $Box.Group = -1
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
    
    foreach ($Box in $Boxes) {
        $Box.Group = -1
    }
    Write-Host "Question 2: Continue connecting the closest unconnected pairs of junction boxes together until they're all in the same circuit. What do you get if you multiply together the X coordinates of the last two junction boxes you need to connect?"
    Write-Host "Answer:", (Part2 -Boxes $Boxes -SortedConnections $SortedConnections)
}

Main

# Question 1: Connect together the 1000 pairs of junction boxes which are closest together. Afterward, what do you get if you multiply together the sizes of the three largest circuits?
# Answer: 57564
# Question 2: Continue connecting the closest unconnected pairs of junction boxes together until they're all in the same circuit. What do you get if you multiply together the X coordinates of the last two junction boxes you need to connect?
# Answer: 133296744
