% SWI-Prolog
%

:- module(shell, [start/0]).
:- [intepreter].
:- [io].

start :-
	cls,
	write('Welcome to the PrologAlmostHaskell Intepreter\n'),
	write('Autor: Michał Zapotoczny\n'),
	parse([])
	.

parse(Env) :-
	write('PAH> '),
	readInstruction(X),
	do(X, Env, REnv),
	parse(REnv)
	.


do([':', 'l', ' '|T], IEnv, Env) :- !,
	loadFromFile(T, IEnv, Env),
	putString(T), write(' loaded'), nl.


do([':', 'q'], _, []) :- !, abort.

do([':', 'e', ' '|Stm], Env, Env) :- !, fromAtomToString(Stm, Stm2), execute(Stm2, Env), nl.

do(N, Env, Env) :- write('Nieznane polecenie: '), putString(N), write('\n'). 

