%  AoC2020, Day 7: Handy Haversacks
%  Author: Chi-Kit Pao
%
%  Commands (used SWI Prolog):
%  swipl day07.pl, or
%  swipl -t halt day07.pl (no interactive mode)
%
%  Outputs:
%  Question 1: How many bag colors can eventually contain at least one shiny gold bag?
%  Answer: 229
%  Question 2: How many individual bags are required inside your single shiny gold bag?
%  Answer: 6683
%

%
%  Time usage shown via command "time":
%  real	0m0,076s
%  user	0m0,073s
%  sys	0m0,004s


:- use_module(library(readutil)).

parse_file(File) :-
    setup_call_cleanup(
        open(File, read, Stream),
        read_lines(Stream),
        close(Stream)
    ).

print_list([]).
print_list([H|T]) :-
    writeln(H),
    print_list(T).

read_lines(Stream) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  true
    ;   parse_line(Line, ContainRules),
        print_list(ContainRules),
        read_lines(Stream)
    ).

% Changed parameter order for currying.
my_split_string(SepChars, PadChars, String, SubStrings) :-
    split_string(String, SepChars, PadChars, SubStrings).

create_type_name(A,B,C) :-
    atom_string(AtomA, A),
    atom_string(AtomB, B),
    atom_concat(AtomA, '_', Atom1),
    atom_concat(Atom1, AtomB, C).

create_contain_rule(BayType, InVal, OutRule) :-
    atom_string(BayTypeAtom, BayType),
    length(InVal, Length),
    (
        Length > 4 ->
        nth0(2, InVal, Name1),
        nth0(3, InVal, Name2),
        nth0(1, InVal, NumberString),
        number_string(Number, NumberString),
        create_type_name(Name1, Name2, TypeNameAtom),
        OutRule = contain(BayTypeAtom, TypeNameAtom, bag(TypeNameAtom, Number)),
        assertz(OutRule)
    ;   nth0(1, InVal, Name1),
        nth0(2, InVal, Name2),
        nth0(0, InVal, NumberString),
        number_string(Number, NumberString),
        create_type_name(Name1, Name2, TypeNameAtom),
        OutRule = contain(BayTypeAtom, TypeNameAtom, bag(TypeNameAtom, Number)),
        assertz(OutRule)
    ).


parse_line(Line, Rules) :-
    string_length(Line, StrLen),
    DesiredLen is StrLen - 1,
    sub_string(Line, 0, DesiredLen, _, Line1),
    re_split(" contain ", Line1, [P1, _ , P2]),
    re_replace(" bags?","", P1, P11),
    re_replace(" ","_", P11, P12),
    (
        P2 == "no other bags" ->
        Rules = []
    ;   split_string(P2, ",", "", P22),
        maplist(my_split_string(" ", ""), P22, P23),
        maplist(create_contain_rule(P12), P23, Rules)
    ).


contain_more(X,Y) :- contain(X,Y,_).
contain_more(X,Y) :- contain(X,Z,_),
    contain_more(Z,Y).

bag_types(BagTypes) :- findall(BagType, contain(BagType,_,_), BagTypes).
contain_shiny_gold(X) :- contain_more(X,shiny_gold).

count_bags(ChildBag,Total) :-
    ChildBag = bag(BagType,Count),
    findall(ChildBag1, contain(BagType,_,ChildBag1), Children),
    maplist(count_bags, Children, L),
    foldl(plus, L, 0, Subtotal),Total is (Subtotal+1)*Count.

question1 :- write("Question 1: How many bag colors can eventually contain at least one shiny gold bag?"),nl.
question2 :- write("Question 2: How many individual bags are required inside your single shiny gold bag?"),nl.
answer1 :- bag_types(BagTypeList),
    list_to_set(BagTypeList, BagTypeSet),
    include(contain_shiny_gold, BagTypeSet, AnswerList1),
    length(AnswerList1, Answer1),
    write("Answer: "),write(Answer1),nl.
main :- parse_file("input.txt"), nl, question1, answer1, question2, answer2.
answer2 :- count_bags(bag(shiny_gold,1), Total), Answer2 is Total-1,
    write("Answer: "),write(Answer2),nl,nl.
?- main.
