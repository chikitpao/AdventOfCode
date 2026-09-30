<# 
    AoC 2020, Day 16: Ticket Translation
    Author: Chi-Kit Pao

    Commands:
    pwsh -ExecutionPolicy ByPass -File "day16.ps1"

    Outputs:
    Question 1: What is your ticket scanning error rate?
    Answer: 22057
    Question 2: Look for the six fields on your ticket that start with the word departure. 
    What do you get if you multiply those six values together?
    Answer: 1093427331937

#>


class Rule {
    $Id = -1
    $Name = ""
    # Start and end values
    $Ranges = [System.Collections.ArrayList]::new()
}

class Quiz {
    $Rules = [System.Collections.ArrayList]::new()
    $MyTicket = $Null
    $NearbyTickets = [System.Collections.ArrayList]::new()
}

function ParseIntString($Line) {
    $Numbers = foreach($Number in $Line.split(",")) {([int]::parse($Number))}
    return $Numbers
}
function ReadQuiz($FileName) {
    $Quiz = [Quiz]::new()
    $StateId = 0
    $CurrentRuleId = 0
    foreach ($Line in Get-Content $FileName) {
        # Example lines
        # departure location: 27-180 or 187-953
        # ...
        # your ticket:
        # 97,61,53,101,131,163,79,103,67,127,71,109,89,107,83,73,113,59,137,139
        # 
        # nearby tickets:
        # 93,566,873,796,908,899,408,393,621,571,546,549,494,631,940,491,561,108,395,258
        # ...

        if($Line -eq "") {
            $StateId += 1
        } else {
            if ($StateId -eq 0) {
                $Parts1R = $Line -split ": "
                if($Line -match "(?<value1>\d+)\-(?<value2>\d+) or (?<value3>\d+)\-(?<value4>\d+)") {
                    $Rule = [Rule]::new()
                    $Rule.Id = $CurrentRuleId
                    $Rule.Name = $Parts1R[0]
                    $Rule.Ranges.Add([int]$Matches["value1"]) | Out-Null
                    $Rule.Ranges.Add([int]$Matches["value2"]) | Out-Null
                    $Rule.Ranges.Add([int]$Matches["value3"]) | Out-Null
                    $Rule.Ranges.Add([int]$Matches["value4"]) | Out-Null
                    $Quiz.Rules.Add($Rule) | Out-Null
                    $CurrentRuleId++
                }
            } elseif ($StateId -eq 1){
                if($Line -ne "your ticket:") {
                    $Quiz.MyTicket = ParseIntString -Line $Line
                }
            } else {
                if($Line -ne "nearby tickets:") {
                    $Numbers = ParseIntString -Line $Line
                    $Quiz.NearbyTickets.Add($Numbers) | Out-Null
                }
            }
        }
    }
    return $Quiz
}

function IsInvalidRuleNumber($Rule, $Number) {
    if(($Number -ge $Rule.Ranges[0] -and $Number -le $Rule.Ranges[1]) -or 
        ($Number -ge $Rule.Ranges[2] -and $Number -le $Rule.Ranges[3])) {
        return $False
    }
    return $True
}

function IsInvalidNumber($Quiz, $Number) {
    foreach($Rule in $Quiz.Rules) {
        if(-not (IsInvalidRuleNumber -Rule $Rule -Number $Number)) {
            return $False
        }
    }
    return $True
}

function Part1($Quiz) {
    $Sum = 0
    foreach($Ticket in $Quiz.NearbyTickets) {
        foreach($Number in $Ticket) {
            if(IsInvalidNumber -Quiz $Quiz -Number $Number) {
                $Sum += $Number
            }
        }
    }
    return $Sum
}
function Part2($Quiz) {
    $ColumnCount = $Quiz.MyTicket.Count
    $TicketColumnRuleIds = [System.Collections.ArrayList]::new()  # Possible Rule IDs in Columns
    $TicketColumnValues = [System.Collections.ArrayList]::new() # Values in Columns
    for ($i = 0; $i -lt $ColumnCount; $i++) {
        $TicketColumnRuleIds.Add([System.Collections.ArrayList]::new()) | Out-Null
        $TicketColumnValues.Add([System.Collections.ArrayList]::new()) | Out-Null
    }

    # Add values of valid tickets to the columns.
    foreach($Ticket in $Quiz.NearbyTickets) {
        $IsInvalidTicket = $False
        foreach($Number in $Ticket) {
            if(IsInvalidNumber -Quiz $Quiz -Number $Number) {
                $IsInvalidTicket = $True
                break
            }
        }

        if(-not $IsInvalidTicket) {
            for ($i = 0; $i -lt $ColumnCount; $i++) {
                $TicketColumnValues[$i].Add($Ticket[$i]) | Out-Null
            }
        }
    }

    # Check column values against rules and find out which rules are still
    # valid for the columns.
    for ($i = 0; $i -lt $ColumnCount; $i++) {
        foreach ($Rule in $Quiz.Rules) {
            $IsInvalidRule = $False
            foreach($Number in $TicketColumnValues[$i]) {
                if(IsInvalidRuleNumber -Rule $Rule -Number $Number) {
                    $IsInvalidRule = $True
                    break
                }
            }
            if(-not $IsInvalidRule) {
                $TicketColumnRuleIds[$i].Add($Rule.Id) | Out-Null
            }
        }
    }

    # Find out unique mapping of rule to column.
    $RuleColumnTable = @{}
    While ($True) {
        $Column = $Null
        $RuleId = $Null
        for ($i = 0; $i -lt $ColumnCount; $i++) {
            if (($TicketColumnRuleIds[$i].Count -eq 1) -and (-not $RuleColumnTable.ContainsKey($TicketColumnRuleIds[$i][0]))) {
                $Column = $i
                $RuleId = $TicketColumnRuleIds[$i][0]
                $RuleColumnTable[$RuleId] = $Column
            }
        }
        if($null -eq $Column -or $null -eq $RuleId) {
            break
        }
        for ($i = 0; $i -lt $ColumnCount; $i++) {
            if ($i -ne $Column) {
                $TicketColumnRuleIds[$i].Remove($RuleId) | Out-Null
            }
        }
    }
    
    # Multiply value of fields which start with "departure".
    $Product = 1
    foreach($e in $RuleColumnTable.GetEnumerator()) {
        if($Quiz.Rules[$e.Name].Name.StartsWith("departure")) {
            $Product *= $Quiz.MyTicket[$e.Value]
        } 
    }
    
    return $Product
}

function Main {
    $Quiz = ReadQuiz("input.txt")

    Write-Host "Question 1: Question 1: What is your ticket scanning error rate?"
    Write-Host "Answer:", (Part1 -Quiz $Quiz)
    Write-Host "Question 2: Look for the six fields on your ticket that start with the word departure. "
        "What do you get if you multiply those six values together?"
    Write-Host "Answer:", (Part2 -Quiz $Quiz)
}

Main
