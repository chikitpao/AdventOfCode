<# 
    Advent of Code 2025
    Day 10: Factory
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day10.ps1"
#>

class TestState{
    [long]$ButtonsPressed
    [long]$State
}

class Machine{
    $LightCount = 0
    $DesiredState = 0
    $Buttons = [System.Collections.ArrayList]::new()
    $JoltageRequirements = [System.Collections.ArrayList]::new()

    [int] Part1() {
        if($this.DesiredState -eq 0) {
            return 0
        }
        $OldStates = [System.Collections.ArrayList]::new()
        $NewStates = [System.Collections.ArrayList]::new()
        for ($i = 0; $i -lt $this.Buttons.Count; $i++) {
            $TestState = [TestState]::new()
            $TestState.ButtonsPressed = 1 -shl $i
            $TestState.State = $this.Buttons[$i]
            $Null = $OldStates.Add($TestState)
            if ($TestState.State -eq $this.DesiredState) {
                return 1
            }
        }
        for($j = 2; $j -lt $this.LightCount; $j++) {
            $TestStateSet = New-Object System.Collections.Generic.HashSet[int]
            foreach ($OldState in $OldStates) {
                for ($i = 0; $i -lt $this.Buttons.Count; $i++) {
                    $BitMask = 1 -shl $i
                    if(($OldState.ButtonsPressed -band $BitMask) -ne 0) {
                        continue
                    }
                    $NewBitMask = $OldState.ButtonsPressed -bor $BitMask
                    if ($TestStateSet.Contains($NewBitMask)) {
                        continue
                    }
                    $Null = $TestStateSet.Add($NewBitMask)
                    $NewState = [TestState]::new()
                    $NewState.ButtonsPressed = $NewBitMask
                    $NewState.State = $this.Buttons[$i] -bxor $OldState.State
                    if($NewState.State -eq $this.DesiredState) {
                        return $j
                    }
                    $Null = $NewStates.Add($NewState)
                }
            }
            if($NewStates.Count -eq 0) {
                break
            }
            $OldStates = $NewStates
            $NewStates = [System.Collections.ArrayList]::new()
        }
        return -1
    }
}

function ParseInput($Line) {
    # Example input
    # [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    $Pos1 = $Line.IndexOf("[")
    $Pos2 = $Line.IndexOf("]")
    $Pos3 = $Line.IndexOf("{")
    $Pos4 = $Line.IndexOf("}")
    $Text1 = $Line.SubString($Pos1 + 1, $Pos2 - $Pos1 - 1)
    $Text2 = $Line.SubString($Pos2 + 2, $Pos3 - $Pos2 - 3)
    $Text3 = $Line.SubString($Pos3 + 1, $Pos4 - $Pos3 - 1)

    $Machine = [Machine]::new()
    $Machine.LightCount = $Text1.Length
    for($i = 0; $i -lt $Text1.Length; $i++) {
        if ($Text1[$i] -eq "#") {
            $Machine.DesiredState = $Machine.DesiredState -bor (1 -shl $i)
        }
    }

    $Buttons = $Text2 -Split " "
    foreach ($Button in $Buttons) {
        $ButtonStateChange = 0
        foreach ($StateChange in ($Button.SubString(1, $Button.Length - 2) -Split ",")) {
            $ButtonStateChange = $ButtonStateChange -bor (1 -shl ([int]$StateChange))
        }
        $Null = $Machine.Buttons.Add($ButtonStateChange)
    }

    $JoltageRequirements = $Text3 -Split ","
    foreach ($JoltageRequirement in $JoltageRequirements) {
        $Null = $Machine.JoltageRequirements.Add([int]$JoltageRequirement)
    }
    return $Machine
}

function Main {
    $Machines = [System.Collections.ArrayList]::new()
    foreach ($Line in Get-Content "input.txt") {
        $Machine = ParseInput($Line)
        $Null = $Machines.Add($Machine)
    }

    $Answer1 = 0
    foreach($Machine in $Machines) {
        $Answer1 += $Machine.Part1()
    }
    Write-Host "Question 1: What is the fewest button presses required to correctly configure the indicator lights on all of the machines?"
    Write-Host "Answer:", $Answer1
}

Main

# Question 1:  What is the fewest button presses required to correctly configure the indicator lights on all of the machines?
# Answer: 538
