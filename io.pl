
% Cls na podstawie http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/8_2.html
cls :-  put(27), put("["), put("2"), put("J"), put(27), put(91), write(0),put(59),write(0), put(72).

green_color :-  put(27), put("["), put("3"), put("2"), put("m").
red_color :-  put(27), put("["), put("3"), put("1"), put("m").
blink_on :- put(27), put("["), put("5"), put("m").
blink_off :- put(27), put("["), put("2"), put("5"), put("m").

reset_color:-  put(27), put("["), put("0"), put("m").
%red_color :- 

readInstruction(X) :- readInstruction(X, []).

readInstruction(X, Acc) :- get_char(H),
							( H = '\n' ->
								reverse(Acc, X)
							;
								( H = end_of_file ->
									fail
								;
									readInstruction(X, [H|Acc])
								)
							).

putString([]) :- !.
putString([H|T]) :- !, put(H), putString(T).
putString(_).

prettyWrite(num(X)) :- !, write(X).
prettyWrite(constr(H)) :- !, write(H).
prettyWrite([constr(H)]) :- !, write(H).
prettyWrite(Code) :-
	isList(Code), 
	Code = [constr(H)|T],
	!,
	write(H), write('('), prettyWriteMany(T), write(')').
prettyWrite(bool(X)) :- !, prettyWrite(X). 

prettyWrite(true) :- !, write('True').
prettyWrite(false) :- !, write('False').
prettyWrite(_) :- !, write('<function>').


prettyWriteMany([]) :- !.
prettyWriteMany([H|T]) :-
	prettyWrite(H), write(' '), prettyWriteMany(T).

fromAtomToString([], []).
fromAtomToString([H|T], [S|SS]) :- name(H, [S]), fromAtomToString(T, SS).

error(X) :- write('Error: '), write(X), nl.
