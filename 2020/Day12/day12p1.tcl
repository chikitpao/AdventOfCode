#! /bin/env tclsh

#   AoC2020, Day 12: Rain Risk, Part 1
#   Author: Chi-Kit Pao
#
#   Output:
#   Question 1: What is the Manhattan distance between that location and the ship's starting position?
#   Answer: 364
#
#   Time usage shown via command "time":
#   real	0m0,018s
#   user	0m0,008s
#   sys	0m0,008s

proc main {} {
    set fp [open "input.txt" r]
    set data [string trim [read $fp]]
    close $fp

    global lines current_state
    set lines [split $data "\n"]

    # current_state
    # east
    # north
    # facing: 0 east, 1 south, 2 west, 3 north
    set current_state 0\ 0\ 0

    set answer1 [find_answer1 $lines]

    puts "Question 1: What is the Manhattan distance between that location and the ship's starting position?"
    puts "Answer: $answer1"
}


proc north { line units } {
    global current_state
    puts "Before $line: $current_state"
    set operand [lindex $current_state 1]
    set result [expr {$operand+$units}]
    set current_state [lreplace $current_state 1 1 $result]
    puts "After $line: $current_state"
}

proc south { line units } {
    global current_state
    puts "Before $line: $current_state"
    set operand [lindex $current_state 1]
    set result [expr {$operand-$units}]
    set current_state [lreplace $current_state 1 1 $result]
    puts "After $line: $current_state"
}

proc east { line units } {
    global current_state
    puts "Before $line: $current_state"
    set operand [lindex $current_state 0]
    set result [expr {$operand+$units}]
    set current_state [lreplace $current_state 0 0 $result]
    puts "After $line: $current_state"
}

proc west { line units } {
    global current_state
    puts "Before $line: $current_state"
    set operand [lindex $current_state 0]
    set result [expr {$operand-$units}]
    set current_state [lreplace $current_state 0 0 $result]
    puts "After $line: $current_state"
}

proc left { line units } {
    global current_state
    puts "Before $line: $current_state"
    switch $units {
        "90" {
            set directions 3\ 0\ 1\ 2
            set old_dir [lindex $current_state 2]
            set current_state [lreplace $current_state 2 2 [lindex $directions $old_dir]]
        }
        "180" {
            set directions 2\ 3\ 0\ 1
            set old_dir [lindex $current_state 2]
            set current_state [lreplace $current_state 2 2 [lindex $directions $old_dir]]
        }
        "270" {
            set directions 1\ 2\ 3\ 0
            set old_dir [lindex $current_state 2]
            set current_state [lreplace $current_state 2 2 [lindex $directions $old_dir]]
        }
        "360" {
            # do nothing
        }
        default {
            throw {CUSTOM_ERROR INVALID_ARGUMENT} "The value provided is not valid."
        }
    }
    puts "After $line: $current_state"
}

proc right { line units } {
    global current_state
    puts "Before $line: $current_state"
    switch $units {
        "90" {
            set directions 1\ 2\ 3\ 0
            set old_dir [lindex $current_state 2]
            set current_state [lreplace $current_state 2 2 [lindex $directions $old_dir]]
        }
        "180" {
            set directions 2\ 3\ 0\ 1
            set old_dir [lindex $current_state 2]
            set current_state [lreplace $current_state 2 2 [lindex $directions $old_dir]]
        }
        "270" {
            set directions 3\ 0\ 1\ 2
            set old_dir [lindex $current_state 2]
            set current_state [lreplace $current_state 2 2 [lindex $directions $old_dir]]
        }
        "360" {
            # do nothing
        }
        default {
            throw {CUSTOM_ERROR INVALID_ARGUMENT} "The value provided is not valid."
        }
    }
    puts "After $line: $current_state"
}

proc forward { line units } {
    global current_state
    puts "Before $line: $current_state"
    set dir [lindex $current_state 2]
    switch $dir {
        "0" {
            set operand [lindex $current_state 0]
            set result [expr {$operand+$units}]
            set current_state [lreplace $current_state 0 0 $result]
        }
        "1" {
            set operand [lindex $current_state 1]
            set result [expr {$operand-$units}]
            set current_state [lreplace $current_state 1 1 $result]
        }
        "2" {
            set operand [lindex $current_state 0]
            set result [expr {$operand-$units}]
            set current_state [lreplace $current_state 0 0 $result]
        }
        "3" {

            set operand [lindex $current_state 1]
            set result [expr {$operand+$units}]
            set current_state [lreplace $current_state 1 1 $result]
        }
        default {
            throw {CUSTOM_ERROR INVALID_ARGUMENT} "The value provided is not valid."
        }
    }
    puts "After $line: $current_state"
}

proc find_answer1 { lines } {
    global current_state
    foreach line $lines {
        set dir [string index $line 0]
        set units [string range $line 1 end]
        switch $dir {
            "N" {
                puts "NORTH $units"
                north $line $units
            }
            "S" {
                puts "SOUTH $units"
                south $line $units
            }
            "E" {
                puts "EAST $units"
                east $line $units
            }
            "W" {
                puts "WEST $units"
                west $line $units
            }
            "L" {
                puts "LEFT $units"
                left $line $units
            }
            "R" {
                puts "RIGHT $units"
                right $line $units
            }
            "F" {
                puts "FORWARD $units"
                forward $line $units
            }
            default {
                throw {CUSTOM_ERROR INVALID_ARGUMENT} "The value provided is not valid."
            }
        }
    }
    set x [lindex $current_state 0]
    set y [lindex $current_state 1]
    return [expr {abs($x)+abs($y)}]
}

main



