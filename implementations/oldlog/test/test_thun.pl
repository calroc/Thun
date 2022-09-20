:- ["../source/thun.pl"].
/* 

Tests

Woefully inadequate, but it's a start.

Run test/0.

    ?- tests.
    YES! test_parser([],[[]])
    YES! test_parser([32],[[]])
    YES! test_parser([91,93],[[list([])]])
    YES! test_parser([50,51],[[int(23)]])
    YES! test_parser([50,91,51,93],[[int(2),list([int(3)])]])
    true.

*/

tests :- forall(test_case(T), test(T)).

test(Goal) :- (Goal -> write("YES! ") ; write("no!  ")), writeln(Goal).

test_parser(Source, Exprs) :- findall(Expr, joy_parse(Expr, Source, []), Exprs).

test_case(test_parser(``, [[]])).
test_case(test_parser(` `, [[]])).
test_case(test_parser(`[]`, [[list([])]])).
test_case(test_parser(`23`, [[ int(23)]])).
test_case(test_parser(`2[3]`, [[int(2), list([int(3)])]])).

% and so on...
