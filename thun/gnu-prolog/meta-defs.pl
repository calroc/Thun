/*
Definitions
*/

do :- assert_defs(`defs.txt`), print_defs, halt.

joy_def(def(Def, Body)) --> symbol(Def), blanks, "==", joy_parse(Body).

joy_def --> joy_def(Def), {ignore(assert_def(Def))}.

joy_defs --> blanks, joy_def, blanks, joy_defs.
joy_defs --> [].


assert_defs(DefsFile) :-
    read_file_to_codes(DefsFile, Codes, []),
    phrase(joy_defs, Codes).

assert_def(def(Def, Body)) :-
    \+ func(Def, _, _),
    \+ combo(Def, _, _, _, _),
    retractall(def(Def, _)),
    assertz(def(Def, Body)).


read_file_to_codes(File, Codes, _) :-
    open(File, read, Stream),
    stream_to_codes(Stream, Codes),
    close(Stream).


stream_to_codes(Stream, Codes) :-
    get_code(Stream, Code),
    stream_to_codes(Code, Stream, Codes).

stream_to_codes(-1,      _,         []) :- !.
stream_to_codes(Ch, Stream, [Ch|Codes]) :- stream_to_codes(Stream, Codes).


print_defs :-
    findall(def(Name, Body), def(Name, Body), List),
    open(`defs.pl`, write, Stream),
    maplist(print_def(Stream), List),
    close(Stream).

print_def(Stream, Def) :- write(Stream, Def), write(Stream, `.`), nl(Stream).


ignore(Goal) :- Goal, !.
ignore(_).

