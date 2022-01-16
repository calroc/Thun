:- use_module(library(md/md_parse)).

fn("C:/Users/sforman/Desktop/src/PROLOG/Thun/docs/reference/Functor-Reference.md").

do(X) :- 
    fn(Fn),
    md_parse_file(Fn, Blocks),
    split_on_hr([_|X], Blocks), !.  % Ignore the header for now  


% Split a list of HTML stuff into sublists on <hr> tags.
split_on_hr([Thing|Rest], Blocks) :- append(Thing, [hr([])|Tail], Blocks), !, split_on_hr(Rest, Tail).
split_on_hr(Blocks, Blocks).


bar([h2(Name)|_]) :- writeln(Name).


fooober(Name, [preable(Preamble)|Sections]) --> [h2(Name)], parts(Preamble), sections(Sections).

sections([S|Rest]) --> section(S), sections(Rest).
sections([]) --> [].

section(definition(Stuff)) --> [h3("Definition")], parts(Stuff).
section(derivation(Stuff)) --> [h3("Derivation")], parts(Stuff).
section(source(Stuff))     --> [h3("Source")],     parts(Stuff).
section(discussion(Stuff)) --> [h3("Discussion")], parts(Stuff).
section(crosslinks(Stuff)) --> [h3("Crosslinks")], parts(Stuff).

parts([P|Ps]) --> part(P), !, parts(Ps).
parts([]) --> [].

part(p(P)) --> [p(P)].
part(pre(P)) --> [pre(P)].

% ... --> [] | [_], ... .

/*

?- do([_, _, X|_]), fooober(Name, Docs, X, _), !.
X = [h2("b"), p([\["(Combinator)"]]), p([\["Run two quoted programs"]]), pre(code("   [P] [Q] b\n---------------\n      P Q")), h3("Definition"), pre(code("[i] dip i")), h3("Derivation"), pre(code(...)), h3(...)|...],
Name = "b",
Docs = [preable([p([\["(Combinator)"]]), p([\["Run two quoted programs"]]), pre(code("   [P] [Q] b\n---------------\n      P Q"))]), definition([pre(code("[i] dip i"))]), derivation([pre(code("[P] [Q] b\n[P] [Q] [i] dip i\n[P] i [Q] i\n P    [Q] i\n P     Q"))]), discussion([p([\[...]])]), crosslinks([p([a(..., ...)|...])])].

 */