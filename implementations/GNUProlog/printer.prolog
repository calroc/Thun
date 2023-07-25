
format_joy_expression(   int(I)) --> { integer(I), !, number_codes(I, Codes) }, Codes.
format_joy_expression(   int(I)) --> { atom(I), !, atom_codes(I, Codes) }, Codes.
format_joy_expression(   int(I)) --> { write_to_codes(Codes, I) }, [40], Codes, [41].
format_joy_expression(  bool(B)) --> { atom_codes(B, Codes) }, Codes.
format_joy_expression(symbol(S)) --> { atom_codes(S, Codes) }, Codes.
format_joy_expression(symbol(S)) --> { atom_codes(S, Codes) }, Codes.
format_joy_expression(  list(J)) --> "[", format_joy_terms(J), "]".

format_joy_terms(    []) --> [].
format_joy_terms(   [T]) --> format_joy_expression(T), !.
format_joy_terms([T|Ts]) --> format_joy_expression(T), " ", format_joy_terms(Ts).

codes_to_stream([Code|Codes], Stream) :-
    put_code(Stream, Code), !,
    codes_to_stream(Codes, Stream).
codes_to_stream([], _).

:- initialization((
    read_term(AST, [end_of_term(eof)]),
    format_joy_terms(AST, Codes, []),
    codes_to_stream(Codes, user_output), print('\n')
    )).


