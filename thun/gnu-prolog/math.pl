func(+, [A, B|C], [D|C]) :-
	E =.. [+, B, A],
	catch(D is E, _, D = E).

func(-, [A, B|C], [D|C]) :-
	E =.. [-, B, A],
	catch(D is E, _, D = E).

func(*, [A, B|C], [D|C]) :-
	E =.. [*, B, A],
	catch(D is E, _, D = E).

func(/, [A, B|C], [D|C]) :-
	E =.. [/, B, A],
	catch(D is E, _, D = E).

func(mod, [A, B|C], [D|C]) :-
	E =.. [mod, B, A],
	catch(D is E, _, D = E).

func(>, [A, B|C], [D|C]) :-
	E =.. [>, B, A],
	catch((E -> D = true ; D = false), _, D = E).

func(<, [A, B|C], [D|C]) :-
	E =.. [<, B, A],
	catch((E -> D = true ; D = false), _, D = E).

func(>=, [A, B|C], [D|C]) :-
	E =.. [>=, B, A],
	catch((E -> D = true ; D = false), _, D = E).

func(<=, [A, B|C], [D|C]) :-
	E =.. [=<, B, A],
	catch((E -> D = true ; D = false), _, D = E).

func(=, [A, B|C], [D|C]) :-
	E =.. [=:=, B, A],
	catch((E -> D = true ; D = false), _, D = E).

func(<>, [A, B|C], [D|C]) :-
	E =.. [=\=, B, A],
	catch((E -> D = true ; D = false), _, D = E).

