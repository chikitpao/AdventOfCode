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
$DeviceSvr = $Null
$DeviceDac = $Null
$DeviceFft = $Null

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
        } elseif (($global:DeviceSvr -eq $Null) -and ($Device.Name -eq "svr")) {
            $global:DeviceSvr = $Device
        } elseif (($global:DeviceDac -eq $Null) -and ($Device.Name -eq "dac")) {
            $global:DeviceDac = $Device
        } elseif (($global:DeviceFft -eq $Null) -and ($Device.Name -eq "fft")) {
            $global:DeviceFft = $Device
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
        } elseif (($global:DeviceSvr -eq $Null) -and ($Device.Name -eq "svr")) {
            $global:DeviceSvr = $Device
        } elseif (($global:DeviceDac -eq $Null) -and ($Device.Name -eq "dac")) {
            $global:DeviceDac = $Device
        } elseif (($global:DeviceFft -eq $Null) -and ($Device.Name -eq "fft")) {
            $global:DeviceFft = $Device
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

function Part1Helper($Devices, $To, $CurrentPath) {
    $Result = 0
    $CurrentDevice = $Devices[$CurrentPath[-1]]
    foreach ($OutId in $CurrentDevice.OutDeviceIds) {
        if ($OutId -eq $To.Id) {
            $Result += 1
        } else {
            $Index = $CurrentPath.IndexOf($OutId)
            if ($Index -ne -1) {
                throw "Recursive Path found!"
            }
            $Null = $CurrentPath.Add($OutId)
            $Result += (Part1Helper -Devices $Devices -To ($Devices[$To.Id]) -CurrentPath $CurrentPath)
            $CurrentPath.RemoveAt($CurrentPath.Count - 1) | Out-Null
        }
    }
    return $Result
}

function Part1($Devices, $From, $To) {
    $CurrentPath = [System.Collections.ArrayList]::new()
    $Null = $CurrentPath.Add($From.Id)
    return Part1Helper -Devices $Devices -To $To -CurrentPath $CurrentPath
}

# Hashtable for Memoization
[hashtable] $CachedResults = @{}

function Part2Helper($Devices, $To, $CurrentPath, $ExclusionList) {
    $CurrentId = $CurrentPath[-1]
    if ($global:CachedResults.Contains($CurrentId)) {
        return $global:CachedResults[$CurrentId]
    }

    $Result = 0
    $CurrentDevice = $Devices[$CurrentId]
    foreach ($OutId in $CurrentDevice.OutDeviceIds) {
        if ($OutId -eq $To.Id) {
            $Result += 1
        } else {
            $Index = $CurrentPath.IndexOf($OutId)
            if ($Index -ne -1) {
                 throw "Recursive Path found!"
            }
            if ($ExclusionList.Contains($OutId)) {
                 continue
            }
            $Null = $CurrentPath.Add($OutId)
            $Result += (Part2Helper -Devices $Devices -To $To -CurrentPath $CurrentPath -ExclusionList $ExclusionList)
            $CurrentPath.RemoveAt($CurrentPath.Count - 1) | Out-Null
        }
    }
    $global:CachedResults[$CurrentId] = $Result
    return $Result
}

function Part2($Devices, $From, $To) {
    $global:CachedResults = @{}
    $CurrentPath = [System.Collections.ArrayList]::new()
    $Null = $CurrentPath.Add($From.Id)
    $ExclusionList = [System.Collections.ArrayList]::new()
    $Null = $ExclusionList.Add($DeviceOut.Id)
    $Null = $ExclusionList.Add($DeviceDac.Id)
    $Null = $ExclusionList.Add($DeviceFft.Id)
    $ExclusionList.Remove($To.Id)
    return Part2Helper -Devices $Devices -To $To -CurrentPath $CurrentPath -ExclusionList $ExclusionList
}

function Main {
    $Devices = ReadDevices("input.txt")

    # Output:
    # Devices.Count: 632
    # DeviceYou.Id: 330
    # DeviceOut.Id: 631
    # DeviceSvr: 214
    # DeviceDac: 538
    # DeviceFft: 206
    Write-Host "Devices.Count: $($Devices.Count)"
    Write-Host "DeviceYou.Id: $($DeviceYou.Id)"
    Write-Host "DeviceOut.Id: $($DeviceOut.Id)"
    Write-Host "DeviceSvr: $($DeviceSvr.Id)"
    Write-Host "DeviceDac: $($DeviceDac.Id)"
    Write-Host "DeviceFft: $($DeviceFft.Id)"

    Write-Host "Question 1: How many different paths lead from you to out?"
    Write-Host "Answer:", (Part1 -Devices $Devices -From $DeviceYou -To $DeviceOut)

    Write-Host "Part2..."
    $DacFft = Part2 -Devices $Devices -From $DeviceDac -To $DeviceFft
    Write-Host "dac -> fft:", $DacFft  # 0
    $FftDac =  Part2 -Devices $Devices -From $DeviceFft -To $DeviceDac
    Write-Host "fft -> dac:", $FftDac  # 6788852
    # These two values must be 0 if $DacFft is 0.
    # Write-Host "svr -> dac:", (Part2 -Devices $Devices -From $DeviceSvr -To $DeviceDac)
    # Write-Host "ftt -> out:", (Part2 -Devices $Devices -From $DeviceFft -To $DeviceOut)
    $SvrFft = Part2 -Devices $Devices -From $DeviceSvr -To $DeviceFft
    Write-Host "svr -> fft:", $SvrFft  # 6351
    $DacOut = Part2 -Devices $Devices -From $DeviceDac -To $DeviceOut
    Write-Host "dac -> out:", $DacOut  # 9676
    
    Write-Host "Question 2: Find all of the paths that lead from svr to out. How many of those paths visit both dac and fft?"
    Write-Host "Answer:", ($SvrFft * $FftDac * $DacOut)
}

Main

# Question 1: How many different paths lead from you to out?
# Answer: 643
# Question 2: Find all of the paths that lead from svr to out. How many of those paths visit both dac and fft?
# Answer: 417190406827152
