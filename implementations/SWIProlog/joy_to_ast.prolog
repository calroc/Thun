
stdin_to_codes(Codes) :-
    % Pass in and discard atom 'code' to prime stdin_to_codes/2.
    stdin_to_codes(code, [code|Codes]).

stdin_to_codes(-1, []) :- !.
stdin_to_codes(Code, [Code|Codes]) :-
    get_code(NextCode),
    stdin_to_codes(NextCode, Codes).

%
%joy(InputString, StackIn, StackOut) :-
%    text_to_expression(InputString, Expression),
%    !,
%    thun(Expression, StackIn, StackOut).
%

joy_lex([tok(Token)|Ls]) --> chars(Token), !, joy_lex(Ls).
joy_lex([  lbracket|Ls]) --> "[",          !, joy_lex(Ls).
joy_lex([  rbracket|Ls]) --> "]",          !, joy_lex(Ls).

joy_lex(Ls) --> blank, !, joy_lex(Ls).

joy_lex([]) --> [].


% Then parse the tokens converting them to Prolog values and building up
% the list structures (if any.)

joy_parse([J|Js]) --> joy_term(J), !, joy_parse(Js).
joy_parse([]) --> [].

joy_term(list(J)) --> [lbracket], !, joy_parse(J), [rbracket].
joy_term(Token) --> [tok(Codes)], {joy_token(Token, Codes)}.

joy_token(int(I), Codes) :- catch(number_codes(I, Codes), _Err, fail), !.
% Leaving the literals below as "true" and "false" caused those
% to be encoded as symbols instead of bools!  I tried '--traditional'
% but then compilation failed with
% > ERROR: atomics_to_string/3: Type error: `text' expected, found `[61]' (a list)
% Which was less helpful than it sounds.
% Anyway, since this is SWI-specific code anyway, why not use ``'s and get on with life?
% https://www.swi-prolog.org/pldoc/man?section=string
joy_token(bool(true), `true`) :- !.
joy_token(bool(false), `false`) :- !.
joy_token(symbol(S), Codes) :- atom_codes(S, Codes).

text_to_expression(Text, Expression) :-
    phrase(joy_lex(Tokens), Text), !,
    phrase(joy_parse(Expression), Tokens).

% Apologies for all the (green, I hope) cuts.  The strength of the Joy
% syntax is that it's uninteresting.

chars([Ch|Rest]) --> char(Ch), chars(Rest).
chars([Ch])      --> char(Ch).

char(Ch) --> \+ blank, [Ch], { Ch \== 0'[, Ch \== 0'] }.


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


main :- 
    stdin_to_codes(Codes),
    text_to_expression(Codes, Expr),
    write_canonical(Expr),
    writeln(".")
    .