/* 
    AoC 2020, Day 18: Operation Order
    Author: Chi-Kit Pao

    Commands:
    groovy day18.groovy

    Outputs:
    Question 1: Evaluate the expression on each line of the homework; what is the sum of the resulting values?
    Answer: 3647606140187
    Question 2: What do you get if you add up the results of evaluating the homework problems using these new rules?
    Answer: 323802071857594

*/

class Token
{
    String token
    long value
    int level // parentheses level, 0-based
}

def createStack (tokens) {
    int maxLevel = -1
    int currentLevel = -1
    def stack = []
    for (token in tokens) {
        switch (token) {
            case "(":
                currentLevel += 1
                maxLevel = Math.max(maxLevel, currentLevel)
                stack.add(new Token(token: token, value: -1, level: currentLevel))
                break
            case ")":
                stack.add(new Token(token: token, value: -1, level: currentLevel))
                currentLevel -= 1
                break
            case "+":
            case "*":
            stack.add(new Token(token: token, value: -1, level: -1))
            break
        default:
            int value = token.toLong()
            stack.add(new Token(token: token, value: value, level: -1))
            break
        }
    }
    return [stack, maxLevel]
}

def eval1(stack, index) {
    if (index >= stack.size())
        return stack
    switch (stack[index].token) {
        case "(":
        case "*":
        case "+":
            eval1(stack, index + 1)
            break
        case ")":
            stack.remove(index)
            stack.remove(index - 2)
            eval1(stack, index - 2)
            break
        default:
            int value = stack[index].value
            if (index == 0 || stack[index - 1].token.equals("(")) {
                eval1(stack, index + 1)
            } else if(stack[index - 1].token.equals("+")) {
                stack.remove(index)
                stack.remove(index - 1)
                def operand = stack.remove(index - 2)
                def newValue = operand.value + value
                def newToken = newValue.toString()
                stack.add(index - 2, new Token(token: newToken, value: newValue, level: -1))
                eval1(stack, index - 2)
            } else if (stack[index - 1].token.equals("*")) {
                stack.remove(index)
                stack.remove(index - 1)
                def operand = stack.remove(index - 2)
                def newValue = operand.value * value
                def newToken = newValue.toString()
                stack.add(index - 2, new Token(token: newToken, value: newValue, level: -1))
                eval1(stack, index - 2)
            } else  {
                def lastToken = stack[index - 1].token
                throw new Exception("Unknown token on stack while handling number! $lastToken");
            }
    }
    return stack
}

def eval2Helper(stack, start, end) {
    for (i = start; i <= end; i++) {
        if(stack[i].token.equals("+")) {
            long newValue = stack[i - 1].value + stack[i + 1].value
            stack.remove(i)
            stack.remove(i)
            stack[i - 1].value = newValue
            stack[i - 1].token = newValue.toString()
            i = i - 1
            end -= 2
        }
    }

    for (i = start; i <= end; i++) {
        if(stack[i].token.equals("*")) {
            long newValue = stack[i - 1].value * stack[i + 1].value
            stack.remove(i)
            stack.remove(i)
            stack[i - 1].value = newValue
            stack[i - 1].token = newValue.toString()
            i = i - 1
            end -= 2
        }
    }
}

def eval2(stack, maxLevel) {
    for (level = maxLevel; level >= 0; level--) {
        for(start = 0; start < stack.size(); start++) {
            if(stack[start].token.equals("(") && stack[start].level == level) {
                for(end = start + 1; end < stack.size(); end++) {
                    if(stack[end].token.equals(")") && stack[end].level == level) {
                        eval2Helper(stack, start + 1, end - 1)
                        stack.remove(start + 2)
                        stack.remove(start)
                        break
                    }
                }
            }
        }
    }
    // level -1
    eval2Helper(stack, 0, stack.size() - 1)

    return stack
}


File file = new File("input.txt")
String line
long answer1 = 0
long answer2 = 0
file.withReader { reader ->
    while ((line = reader.readLine()) != null) {
        String s1 = line.replaceAll("\\(", "\\( ").replaceAll("\\)", " \\)")
        def tokens = s1.tokenize(" ")
        def stack
        def maxLevel
        (stack, maxLevel) = createStack(tokens)
        def result = eval1(stack, 0)
        def value = result[0].value
        answer1 += value

        (stack, maxLevel) = createStack(tokens)
        result = eval2(stack, maxLevel)
        value = result[0].value
        answer2 += value
    }
}

println "Question 1: Evaluate the expression on each line of the homework; what is the sum of the resulting values?"
println "Answer: " + answer1
println "Question 2: What do you get if you add up the results of evaluating the homework problems using these new rules?"
println "Answer: " + answer2