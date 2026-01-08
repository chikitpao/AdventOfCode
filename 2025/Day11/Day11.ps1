<# 
    Advent of Code 2025
    Day 11: Reactor
    Author: Chi-Kit Pao
    pwsh -ExecutionPolicy ByPass -File "Day11.ps1"
#>

class Device {
    $Id = -1
    $Name = ""
    $OutDeviceNames = [System.Collections.ArrayList]::new()
    $OutDeviceIds = [System.Collections.ArrayList]::new()
}

$DeviceYou = $Null
$DeviceOut = $Null

function ReadDevices($FileName) {
    $Devices = [System.Collections.ArrayList]::new()
    $DevicesCreated = New-Object System.Collections.Generic.HashSet[String]
    $DevicesNew = New-Object System.Collections.Generic.HashSet[String]
    [hashtable] $NameToId = @{}

    $CurrentId = 0
    foreach ($Line in Get-Content $FileName) {
        $ColonPos = $Line.IndexOf(":")
        $Device = [Device]::new()
        $Device.Id = $CurrentId
        $Device.Name = $Line.Substring(0, $ColonPos)
        $NameToId[$Device.Name] = $Device.Id
        $Null = $DevicesCreated.Add($Device.Name)
        $Null = $DevicesNew.Remove($Device.Name)
        if (($global:DeviceYou -eq $Null) -and ($Device.Name -eq "you")) {
            $global:DeviceYou = $Device
        } elseif (($global:DeviceOut -eq $Null) -and ($Device.Name -eq "out")) {
            $global:DeviceOut = $Device
        }
        $OutDevices = $Line.Substring($ColonPos + 2) -Split " "
        foreach ($OutDevice in $OutDevices) {
            if (-not ($DevicesCreated.Contains($OutDevice))) {
                $Null = $DevicesNew.Add($OutDevice)
            }
            $Null = $Device.OutDeviceNames.Add($OutDevice)
        }
        $Null = $Devices.Add($Device)
        $CurrentId += 1
    }

    foreach ($DeviceName in $DevicesNew) {
        $Device = [Device]::new()
        $Device.Id = $CurrentId
        $Device.Name = $DeviceName
        $NameToId[$Device.Name] = $Device.Id
        if (($global:DeviceYou -eq $Null) -and ($Device.Name -eq "you")) {
            $global:DeviceYou = $Device
        } elseif (($global:DeviceOut -eq $Null) -and ($Device.Name -eq "out")) {
            $global:DeviceOut = $Device
        }
        # No output for this device
        $Null = $Devices.Add($Device)
        $CurrentId += 1
    }

    foreach ($Device in $Devices) {
        foreach ($Name in $Device.OutDeviceNames) {
            $Null = $Device.OutDeviceIds.Add($NameToId[$Name])
        }
    }

    return $Devices
}

function Part1Helper($Devices, $CurrentPath) {
    $Result = 0
    $CurrentDevice = $Devices[$CurrentPath[-1]]
    foreach ($OutId in $CurrentDevice.OutDeviceIds) {
        if ($OutId -eq $global:DeviceOut.Id) {
            $Result += 1
        } else {
            $Index = $CurrentPath.IndexOf($OutId)
            if ($Index -ne -1) {
                throw "Recursive Path found!"
            }
            $Null = $CurrentPath.Add($OutId)
            $Result += (Part1Helper -Devices $Devices -CurrentPath $CurrentPath)
            $CurrentPath.RemoveAt($CurrentPath.Count - 1) | Out-Null
        }
    }
    return $Result
}

function Part1($Devices) {
    $CurrentPath = [System.Collections.ArrayList]::new()
    $Null = $CurrentPath.Add($global:DeviceYou.Id)
    return Part1Helper -Devices $Devices -CurrentPath $CurrentPath
}

function Main {
    $Devices = ReadDevices("input.txt")
    Write-Host "Devices.Count: $($Devices.Count)"
    Write-Host "DeviceYou.Id: $($DeviceYou.Id)"
    Write-Host "DeviceOut.Id: $($DeviceOut.Id)"

    Write-Host "Question 1: How many different paths lead from you to out?"
    Write-Host "Answer:", (Part1 -Devices $Devices)
}

Main

# Question 1:  Question 1: How many different paths lead from you to out?
# Answer: 643
