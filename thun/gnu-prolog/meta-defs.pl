/*
Definitions
*/

do :- assert_defs(`defs.txt`), print_defs, halt.

joy_def(def(Def, Body)) --> symbol(Def), blanks, "==", joy_parse(Body).

joy_defs --> blanks, joy_def(Def), {assert_def(Def)}, blanks, joy_defs.
joy_defs --> [].

assert_defs(DefsFile) :-
    read_file_to_codes(DefsFile, Codes, []),
    phrase(joy_defs, Codes).

assert_def(def(Def, Body)) :-
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
    maplist(print_def, List).

print_def(Def) :- write(Def), write(`.`), nl.

