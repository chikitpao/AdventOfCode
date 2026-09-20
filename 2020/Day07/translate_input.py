#  AoC2020, Day 7: Handy Haversacks
#  Author: Chi-Kit Pao
#  Use current Python script to parse input and create a Prolog program. Then
#  solve the puzzle with SWI Prolog.
#  Commands:
#  python3 translate_input.py > day07.pl
#  swipl day07.pl
#
#  Outputs:
#  Question 1: How many bag colors can eventually contain at least one shiny gold bag?
#  Answer: 229
#  Question 2: How many individual bags are required inside your single shiny gold bag?
#  Answer: 6683
#

import os

def parse(line):
    p1, p2 = line.split(" contain ")
    p1 = p1.replace(" bags", "")
    p1 = p1.replace(" bag", "")
    p1 = p1.replace(" ", "_")
    if p2 == "no other bags.":
        return p1, []
    else:
        p2 = p2.replace(".", "")
        p2 = p2.replace(" bags", "")
        p2 = p2.replace(" bag", "")
        p2 = p2.replace(" ", "_")
        l = p2.split(",_")
        contained_bags = list(map(lambda s: ( int(s[:s.index("_")]), s[s.index("_")+1:]), l))
        return p1, contained_bags

def print_prolog_file(bags):
    print("contain_more(X,Y) :- contain(X,Y,_).")
    print("contain_more(X,Y) :- contain(X,Z,_),")
    print("\tcontain_more(Z,Y).\n")

    for b, c in bags.items():
        if not c:
            print(f"% {b} is empty.")
        else:
            for ce in c:
                print(f"contain({b},{ce[1]},bag({ce[1]},{ce[0]})).")

    print("\ncontain_shiny_gold(X) :- contain_more(X,shiny_gold).\n")

    print("count_bags(ChildBag,Total) :-")
    print("    ChildBag = bag(BagType,Count),")
    print("    findall(ChildBag1, contain(BagType,_,ChildBag1), Children),")
    print("    maplist(count_bags, Children, L),")
    print("    foldl(plus, L, 0, Subtotal),Total is (Subtotal+1)*Count.\n")


    bag_list = ",".join(bags.keys())
    print("question1 :- write(\"Question 1: How many bag colors can eventually contain at least one shiny gold bag?\"),nl.")
    print("question2 :- write(\"Question 2: How many individual bags are required inside your single shiny gold bag?\"),nl.")
    print("answer1 :- include(contain_shiny_gold, [" + bag_list + "], AnswerList1),\n\tlength(AnswerList1, Answer1),")
    print("\twrite(\"Answer: \"),write(Answer1),nl.")
    print("main :- question1, answer1, question2, answer2.")
    print("answer2 :- count_bags(bag(shiny_gold,1), Total), Answer2 is Total-1,")
    print("\twrite(\"Answer: \"),write(Answer2),nl,nl.")
    print("?- main.")

def main():
    file_path = os.path.dirname(__file__)
    bags = dict()
    with open(os.path.join(file_path, "input.txt")) as f:
        lines = list(map(lambda s: s.replace("\n", ""), f.readlines()))
        for line in lines:
            name, contained_bags = parse(line)
            bags[name] = contained_bags
    print_prolog_file(bags)

if __name__ == "__main__":
    main()