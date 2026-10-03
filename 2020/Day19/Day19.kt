// AoC 2020 Day 19: Monster Message
// Author: Chi-Kit Pao
//
// Outputs:
// Question 1: How many messages completely match rule 0?
// Answer: 216
// Question 2: After updating rules 8 and 11, how many messages completely match rule 0?
// Answer: 400
//

import java.io.File
import kotlin.collections.mutableListOf

data class Rule(val id: Int) {
    val replacements: ArrayList<ArrayList<kotlin.Any>> = arrayListOf()
}

fun createRules(ruleTextList: List<String>): HashMap<Int, Rule> {
    var ruleMap = HashMap<Int, Rule>()
    for (line in ruleTextList) {
        val as1 : Array<String?>? = line.split(": ").toTypedArray()
        val id = as1!!.get(0)!!.toInt()
        val newRule = Rule(id)
        val as2 : Array<String> = as1!!.get(1)!!.split(" | ").toTypedArray()
        for (s in as2) {
            if (s.contains("\"")){
                val replacement = ArrayList<Any>()
                replacement.add(s.substring(1,2))
                newRule.replacements.add(replacement)
            } else {
                val sl = s.split(" ").toTypedArray()
                val il = sl.map{it.toInt()}
                val replacement = ArrayList<Any>()
                for (i in il)
                    replacement.add(i)
                newRule.replacements.add(replacement)
            }
        }
        ruleMap[id] = newRule
    }
    return ruleMap
}

fun parseMessage(ruleMap: HashMap<Int, Rule>, s: String, ruleId: Int, indices: List<Int>): List<Int> {
    if (indices.isEmpty())
        return ArrayList()

    val rule = ruleMap[ruleId] ?: return ArrayList()

    val returnList = ArrayList<Int>()
    for (replacement in rule.replacements) {
        var currentIndices = indices.map { it }
        var newIndices = ArrayList<Int>()
        for (r in replacement) {
            if (r is String) {
                for (i in currentIndices) {
                    val end = i + r.length
                    if (end <= s.length && s.substring(i, end).equals(r)) {
                        newIndices.add(end)
                    }
                }
            } else if (r is Int) {
                newIndices.addAll(parseMessage(ruleMap, s, r, currentIndices))
            }
            currentIndices = newIndices.map {it}
            newIndices = ArrayList()
        }
        returnList.addAll(currentIndices)
    }
    return returnList
}

fun checkMessages(ruleMap: HashMap<Int, Rule>, messageList: List<String>): Int {
    var answer = 0
    for (s in messageList) {
        val indices = ArrayList<Int>()
        indices.add(0)
        val nextIndices = parseMessage(ruleMap, s, 0, indices)
        if (nextIndices.any { it == s.length })
            answer += 1
    }
    return answer
}

fun main() {
    val ruleTextList = mutableListOf<String>()
    val messageList = mutableListOf<String>()
    var stateId = 0

    // Read rules and messages
    File("input.txt").useLines {
        lines -> lines.forEach {
            if (it.equals("")) {
                stateId += 1
            } else if (stateId == 0) {
                ruleTextList.add(it)
            } else if (stateId == 1) {
                messageList.add(it)
            }
        }
    }

    // Convert rules to Rule objects
    var ruleMap = createRules(ruleTextList)

    // Part 1
    println("Question 1: How many messages completely match rule 0?")
    println("Answer: " + checkMessages(ruleMap, messageList))

    // Part 2: Replace the following rules
    // 8: 42 | 42 8
    // 11: 42 31 | 42 11 31
    val rule8 = Rule(8)
    rule8.replacements.add(arrayListOf(42))
    rule8.replacements.add(arrayListOf(42, 8))
    ruleMap[8] = rule8
    val rule11 = Rule(11)
    rule11.replacements.add(arrayListOf(42, 31))
    rule11.replacements.add(arrayListOf(42, 11, 31))
    ruleMap[11] = rule11
    println("Question 2: After updating rules 8 and 11, how many messages completely match rule 0?")
    println("Answer: " + checkMessages(ruleMap, messageList))
}