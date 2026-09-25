#! /bin/env tclsh

#   AoC2020, Day 12: Rain Risk, Part 2
#   Author: Chi-Kit Pao
#
#   Output:
#   Question 2: What is the Manhattan distance between that location and the ship's starting position?
#   Answer: 39518
#
#   Time usage shown via command "time":
#   real	0m0,020s
#   user	0m0,016s
#   sys	0m0,001s


proc main {} {
    set fp [open "input.txt" r]
    set data [string trim [read $fp]]
    close $fp

    global lines current_state
    set lines [split $data "\n"]

    # current_state
    # ship east
    # ship northnorth
    # waypoint east
    # waypoint north
    set current_state 0\ 0\ 10\ 1

    set answer2 [find_answer2 $lines]

    puts "Question 2: What is the Manhattan distance between that location and the ship's starting position?"
    puts "Answer: $answer2"
}


proc north { line units } {
    global current_state
    puts "Before $line: $current_state"
    set operand [lindex $current_state 3]
    set result [expr {$operand+$units}]
    set current_state [lreplace $current_state 3 3 $result]
    puts "After $line: $current_state"
}

proc south { line units } {
    global current_state
    puts "Before $line: $current_state"
    set operand [lindex $current_state 3]
    set result [expr {$operand-$units}]
    set current_state [lreplace $current_state 3 3 $result]
    puts "After $line: $current_state"
}

proc east { line units } {
    global current_state
    puts "Before $line: $current_state"
    set operand [lindex $current_state 2]
    set result [expr {$operand+$units}]
    set current_state [lreplace $current_state 2 2 $result]
    puts "After $line: $current_state"
}

proc west { line units } {
    global current_state
    puts "Before $line: $current_state"
    set operand [lindex $current_state 2]
    set result [expr {$operand-$units}]
    set current_state [lreplace $current_state 2 2 $result]
    puts "After $line: $current_state"
}

proc left { line units } {
    global current_state
    puts "Before $line: $current_state"
    set ship_east [lindex $current_state 0]
    set ship_north [lindex $current_state 1]
    set old_offset_east [expr {[lindex $current_state 2]-[lindex $current_state 0]}]
    set old_offset_north [expr {[lindex $current_state 3]-[lindex $current_state 1]}]
    switch $units {
        "90" {
            set new_offset_east [expr {-$old_offset_north}]
            set new_offset_north $old_offset_east
            set current_state $ship_east\ $ship_north\ [expr {$ship_east+$new_offset_east}]\ [expr {$ship_north+$new_offset_north}]
        }
        "180" {
            set new_offset_east [expr {-$old_offset_east}]
            set new_offset_north [expr {-$old_offset_north}]
            set current_state $ship_east\ $ship_north\ [expr {$ship_east+$new_offset_east}]\ [expr {$ship_north+$new_offset_north}]
        }
        "270" {
            set new_offset_east [expr {$old_offset_north}]
            set new_offset_north [expr {-$old_offset_east}]
            set current_state $ship_east\ $ship_north\ [expr {$ship_east+$new_offset_east}]\ [expr {$ship_north+$new_offset_north}]
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
    set ship_east [lindex $current_state 0]
    set ship_north [lindex $current_state 1]
    set old_offset_east [expr {[lindex $current_state 2]-[lindex $current_state 0]}]
    set old_offset_north [expr {[lindex $current_state 3]-[lindex $current_state 1]}]
    switch $units {
        "90" {
            set new_offset_east [expr {$old_offset_north}]
            set new_offset_north [expr {-$old_offset_east}]
            set current_state $ship_east\ $ship_north\ [expr {$ship_east+$new_offset_east}]\ [expr {$ship_north+$new_offset_north}]
        }
        "180" {
            set new_offset_east [expr {-$old_offset_east}]
            set new_offset_north [expr {-$old_offset_north}]
            set current_state $ship_east\ $ship_north\ [expr {$ship_east+$new_offset_east}]\ [expr {$ship_north+$new_offset_north}]
        }
        "270" {
            set new_offset_east [expr {-$old_offset_north}]
            set new_offset_north $old_offset_east
            set current_state $ship_east\ $ship_north\ [expr {$ship_east+$new_offset_east}]\ [expr {$ship_north+$new_offset_north}]
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
    set old_ship_east [lindex $current_state 0]
    set old_ship_north [lindex $current_state 1]
    set old_offset_east [expr {[lindex $current_state 2]-[lindex $current_state 0]}]
    set old_offset_north [expr {[lindex $current_state 3]-[lindex $current_state 1]}]
    set new_ship_east [expr {$old_ship_east+$units*$old_offset_east}]
    set new_ship_north [expr {$old_ship_north+$units*$old_offset_north}]
    set waypoint_east [expr {[lindex $current_state 2]*$units}]
    set waypoint_north [expr {[lindex $current_state 3]*$units}]
    set current_state $new_ship_east\ $new_ship_north\ [expr {$new_ship_east+$old_offset_east}]\ [expr {$new_ship_north+$old_offset_north}]
    puts "After $line: $current_state"
}

proc find_answer2 { lines } {
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
