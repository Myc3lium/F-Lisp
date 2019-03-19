

postOrder(s_expression(Functor, Args)) -->
	phraseMap(postOrder, Args),
	postOrder(Functor).


postOrder([X | Xs]) -->
	listOrder([X | Xs]).

postOrder(X) --> [X].

% listOrder(+List).
listOrder([]) --> [[]].
listOrder([X|Xs]) -->
	listOrder(Xs),
	postOrder(X),
	[cons].


phraseMap(_, []) --> [].
phraseMap(Indicator, [Head | Tail], As, Bs) :-
	call(Indicator, Head, As, Cs),
	phraseMap(Indicator, Tail, Cs, Bs).

writeSep(Term) :- write(Term), write(' ').


func_compile(Name, Args, Body) :-
	writeSep(':'),
		writeSep(Name),
	writeSep('{'),
		maplist(writeSep, Args),
	writeSep('}'),
	maplist(compile, Body),
	writeSep(';').

cond_compile(Switch, If, Else) :-
	compile(Switch),
	writeSep('IF'),
	compile(If),
	writeSep('THEN'),
	compile(Else).


compile(s_expression('Program', Body)) :-
	maplist(compile, Body).

compile(s_expression(def, [s_expression(Name, Args) | Body])) :-
	func_compile(Name, Args, Body).

compile(s_expression(if,  [Cond, If, Else])) :-
	cond_compile(Cond, If, Else).

% Base case.
compile(s_expression(Functor, Args)) :-
	phrase(postOrder(s_expression(Functor, Args)), Output, []),
	maplist(writeSep, Output).


compile(X) :- writeSep(X).


compile_s(String) :-
	phrase(s_expression(Tree), String),
	compile(Tree),
	!.

compile_s(String) :-
	phrase(s_arguments(Tree), String),
	compile( s_expression('Program', Tree) ),
	!.

