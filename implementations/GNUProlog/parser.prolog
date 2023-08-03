

text_to_expression(Text, Expression) :-
    phrase(joy_lex(Tokens), Text), !,
    phrase(joy_parse(Expression), Tokens).


joy_lex([tok(Token)|Ls]) --> chars(Token), !, joy_lex(Ls).
joy_lex([  lbracket|Ls]) --> "[",          !, joy_lex(Ls).
joy_lex([  rbracket|Ls]) --> "]",          !, joy_lex(Ls).
joy_lex(            Ls ) --> blank,        !, joy_lex(Ls).

joy_lex([]) --> [].


joy_parse([J|Js]) --> joy_term(J), !, joy_parse(Js).
joy_parse([]) --> [].


joy_term(list(J)) --> [lbracket], !, joy_parse(J), [rbracket].
joy_term(Term) --> [tok(Codes)], { joy_term(Term, Codes) }.


joy_term(     int(I),   Codes) :- numeric(Codes), !, atom_codes(I, Codes).
joy_term( bool(true),  "true") :- !.
joy_term(bool(false), "false") :- !.
joy_term(  symbol(S),   Codes) :- atom_codes(S, Codes).


% Apologies for all the (green, I hope) cuts.  The strength of the Joy
% syntax is that it's uninteresting.

chars([Ch|Rest]) --> char(Ch), chars(Rest).
chars([Ch])      --> char(Ch).

char(Ch) --> \+ blank, [Ch], { Ch \== 0'[, Ch \== 0'] }.

numeric(Codes) :- digits(Codes, []), !.
numeric([45,FirstDigit|Codes]) :- digit(FirstDigit), digits(Codes, []), !.
% ASCII 45 is '-'.

digits --> digit, digits.
digits --> [].

digit --> [Code], { digit(Code) }.

digit(Code) :- between(0'0, 0'9, Code).


% TODO: code golf this into something more efficient.
blank --> [9].
blank --> [10].
blank --> [11].
blank --> [12].
blank --> [13].
blank --> [32].
blank --> [194, 133].
blank --> [194, 160].
blank --> [225, 154, 128].
blank --> [226, 128, 128].
blank --> [226, 128, 129].
blank --> [226, 128, 130].
blank --> [226, 128, 131].
blank --> [226, 128, 132].
blank --> [226, 128, 133].
blank --> [226, 128, 134].
blank --> [226, 128, 135].
blank --> [226, 128, 136].
blank --> [226, 128, 137].
blank --> [226, 128, 138].
blank --> [226, 128, 168].
blank --> [226, 128, 169].
blank --> [226, 128, 175].
blank --> [226, 129, 159].
blank --> [227, 128, 128].


stdin_to_codes(Codes) :- stdin_to_codes(code, [code|Codes]).
% Pass in and discard atom 'code' to prime stdin_to_codes/2.    

stdin_to_codes(-1, []) :- !.
stdin_to_codes(Code, [Code|Codes]) :-
    get_code(NextCode),
    stdin_to_codes(NextCode, Codes).


:- initialization((
    stdin_to_codes(Codes),
    text_to_expression(Codes, Expr),
    write_term(Expr, [quoted(true)]), print('.\n')
    )).
