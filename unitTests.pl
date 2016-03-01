:- [parser].
:- [intepreter].
:- [io].

% Z powodu braku czasu tylko tyle testów, ale wykorzystują one różne funkcjonalności języka
units(X) :- X = 
[	
	("ack 2 2", num(7)),
	
	("and False (1 div 0)", bool(false)), 

	("length (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))", num(4)),

	("take 5 ones", [constr(Cons),num(1),[constr(Cons),num(1),[constr(Cons),num(1),[constr(Cons),num(1),[constr(Cons),num(1),[constr(Nil)]]]]]]),

	("sieve (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))", [constr(Cons),num(3),[constr(Cons), num(5), [constr(Nil)]]]),

	("take 10 fib", [constr(Cons),num(1),[constr(Cons),num(1),[constr(Cons),num(2),[constr(Cons),num(3),[constr(Cons),num(5),[constr(Cons),num(8),[constr(Cons),num(13),[constr(Cons),num(21),[constr(Cons),num(34),[constr(Cons),num(55),[constr(Nil)]]]]]]]]]]]),

	("sort (Cons 123 (Cons 100 (Cons 5 Nil)))", [constr(Cons),num(5),[constr(Cons),num(100),[constr(Cons),num(123),constr(Nil)]]]),

	("take 5 primes", [constr(Cons),num(2),[constr(Cons),num(3),[constr(Cons),num(5),[constr(Cons),num(7),[constr(Cons),num(11),[constr(Nil)]]]]]])
].	

test([], _).
test([(In, Out)|Rest], Env) :-
	parseStm(In, Parsed),
	write('Test '), putString(In), flush,
	( runWholeCode(Parsed, Env, Result) ->
		( Out = Result ->
			 green_color, write('   PASSED'), reset_color, nl
		;
			 red_color, blink_on, write('   FAILED (Wrong answer)'), blink_off, reset_color, nl,
			write('\t\tWas     : '), write(Result), nl,
			write('\t\tExpected: '), write(Out), nl
		)
	;
		write('Test '), putString(In), write('   FAILED (Terminated)'), nl
	),
	test(Rest, Env).


start :-
	cls,
	write('Welcome to PAH UnitTests. Now I will load unitTests.pah'), nl,
	%parseFromFile("newUnitTests.pah", ParsedProg),
	loadFromFile('newUnitTests.pah', [], Env),
	write('Starting tests'),nl,
	units(Units),
	test(Units, Env).

