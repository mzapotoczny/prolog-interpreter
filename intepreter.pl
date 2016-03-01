/*
* Moduł intepretera PAH
* 
*/


%:- module(intepreter, [loadFromFile/3, execute/2, runWholeCode/3]).
:- dynamic(runCodeWithRemembering/3).
:- dynamic(runCode/4).
:- [parser].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Funkcje pomocnicze
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isList([_|_]).

unpack([X], R) :- unpack(X, R), !.
unpack(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Generowanie środowiska funkcji
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Czyści środowisko z funkcji które będą władowane od nowa
removeDuplicatedFunctions(_, [], []) :- !.
removeDuplicatedFunctions([], E, E) :- !.
removeDuplicatedFunctions([func(N, _, _, _)|T], OldEnv, NewEnv) :-
	findAndDestroy(N, OldEnv, TempEnv),
	removeDuplicatedFunctions(T, TempEnv, NewEnv).

findAndDestroy(_, [], []) :- !.
findAndDestroy(Name, [(Name, _)|R], Rest) :- findAndDestroy(Name, R, Rest), !.
findAndDestroy(Name, [(Namae, _)|R], Rest) :- name(Namae, TName), name(Name, OName), append(OName, [_,64], OOName),  append(OOName, _, TName), findAndDestroy(Name, R, Rest), !.
findAndDestroy(Name, [F|R], [F|Rest]) :- findAndDestroy(Name, R, Rest).

% Generuje środowisko
% loadEnv :: Lista wygenerowana przez parser -> środowsiko
loadEnv(L, PEnv, Env) :- loadEnv('', L, PEnv,Env, [], _). 

% Właściwy predykat generujący środowisko
% loadEnv :: Prefix (String) -> Lista wygenerowana przez parser -> akumulator -> środowisko wynikowe -> akumulator 2 -> podstawienie f. lokalnych

loadEnv(_, [], Env, Env, CA, CA) :- !.

% Wyjaśnienie formatu: func(Nazwa funkcji, Pattern, Ciało funkcji, Deklaracje lokalne)
loadEnv(Prefix, [func(N, P, Bpp, E)|T], ACC, Env, ChangesACC, Changes) :- 
	concat(Prefix, N, FuncName),
	( Prefix = '' ->
		NewChangesACC = ChangesACC,
		Bp = Bpp
	;
		betaReduction(Bpp, [(N, FuncName)], Bp),
		(member( (N, FuncName), ChangesACC ) ->
			NewChangesACC = ChangesACC
		;
			NewChangesACC = [(N, FuncName)|ChangesACC]
		)
	),

	( member((FuncName, Func), ACC) -> 
		length(Func, PatNum)
	;
		PatNum = 0
	),

	( \+ E = [] ->
		% Wygenerujmy nowy prefiks!
		% Stary prefiks ++ numer dopasowania ++ @ ++ nazwa funkcji 
		concat(FuncName, PatNum, NP1),
		concat(NP1, '@', NewPrefix),
		loadEnv(NewPrefix, E, ACC, NACC, [], ChList),
		betaReduction(Bp, ChList, B)
	;
		NACC = ACC,
		B = Bp
	),

	( PatNum = 0 ->
		NACC1 = [(FuncName, [fnc(P,B)])|NACC]
	;
		append(Func, [fnc(P, B)], Replacement),
		replace(FuncName, Func, Replacement, NACC, NACC1)
	),		

	loadEnv(Prefix, T, NACC1, Env, NewChangesACC, Changes), !.

loadEnv(A, B, C, D, E, F) :- \+ isList(B), loadEnv(A, [B], C, D, E, F).

replace(N, Z1, Z2, [(N, Z1)|R1], [(N, Z2)|R1]) :- !.
replace(N, Z1, Z2, [(N1, Z11)|R1], [(N1, Z11)|R2]) :- N1 \= N, !, replace(N, Z1, Z2, R1, R2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Pattern matching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Znaleźliśmy zbiór ciał funkcji o szukanej nazwie
findFuncAndMatchPattern(Func, Rest, [(Func, Z)|_], Env, Result) :-
	!, 
	matchPattern(Rest, Z, Env, Result).	
findFuncAndMatchPattern(Func, Rest, [_|R], Env, Result) :- findFuncAndMatchPattern(Func, Rest, R, Env, Result).

% Szukamy teraz ciała pasującego do naszego wywołania
matchPattern(Rest, [Func|_], Env, Result) :-
	Func = fnc(Pattern, Body),
	matchPatternStage2(Rest, Pattern, Env, Res, ToAPP),
	append(Body, ToAPP, FuncBody),
	Function = fnc(Pattern, FuncBody),
	Result = patterned(Function, Res), !.

matchPattern(Rest, [_|R], Env, Result) :-
	matchPattern(Rest, R, Env, Result), !.
matchPattern(_, [], _, []) :- !.


matchPatternStage2([], [], _, [], []) :- !.

% Mamy prawdopodobnie częściową aplikację
matchPatternStage2([H|T], [], _, [], [H|T]) :- !.

% Sprowadźmy przypadek szczególny do ogólnego
matchPatternStage2(X, Y, Env, R, Rest) :-
	\+ isList(X), !,
	matchPatternStage2([X], Y, Env, R, Rest).

matchPatternStage2([P1|R1], [P2|R2], Env, Result, Rest) :-
	matchOnePattern(P1, P2, Env, RES1),
	matchPatternStage2(R1, R2, Env, RES2, Rest),
	( isList(RES1) ->
		RES1P = RES1
	;
		RES1P = [RES1]
	),
	append(RES1P, RES2, Result).


% Pattern matching wg. tabeli z opisu ogólnego
matchOnePattern(What, variable(X), _, (X, What)) :- !.
matchOnePattern(num(X), num(X), _, []) :- !.
matchOnePattern(Z, num(X), Env, []) :- runCodeWithRemembering(Z, Env, R), !, (X = R; num(X) = R).
matchOnePattern(Z, bool(X), Env, []) :-  runCodeWithRemembering(Z, Env,  R), !, (bool(X) = R; X = R).

matchOnePattern(constr(N), constr(N), _, []).

matchOnePattern([constr(N)|[Params]], (constr(N),MParams), Env, Result) :-
	matchPatternStage2(Params, MParams, Env, Result, []), !.

matchOnePattern(Z, constr(N), Env, Result) :-
	runCode(Z, Env, RetVal, _),
	(RetVal = result([constr(N)]); RetVal = constr(N); RetVal = result(constr(N))), !, Result = [].

matchOnePattern(Func, (constr(N),MParams), Env, Result) :-
	runCode(Func, Env, RetVal, _),
	RetVal =[constr(N)|Params],
	matchPatternStage2(Params, MParams, Env, Result, []), !.

matchOnePattern([constr(N)|_], constr(N), _, _) :- fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  β-redukcja 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

findAndReplace(_, _, [], []) :- !.
findAndReplace(N, NN, [H|T], Res) :-
	isList(H), !,
	findAndReplace(N, NN, H, ResP),
	findAndReplace(N, NN, T, ResP2),
	Res = [ResP|ResP2].

findAndReplace(Needle, NewNeedle, [H|T], Result) :-
	(
		H = Needle,
		Hp = NewNeedle, !
	;
		H = [Needle],
		Hp = [NewNeedle], !
	;
		H = math(Op, Left, Right),
		findAndReplace(Needle, NewNeedle, Left, R1),
		findAndReplace(Needle, NewNeedle, Right, R2),
		Hp = math(Op, R1, R2),
		!
	;
		H = lambda(Var, Body),
		% Jeśli zmienna którą aktualizujemy jest taka sama jak ta w lambdzie,
		% to nie nie nadpisujemy tego, w p.p możemy wejść do ciała lambdy
		( (Var = Needle; unpack(Needle, Var) ) ->
			R1 = Body
		;
			findAndReplace(Needle, NewNeedle, Body, R1)
		),
		Hp = lambda(Var, R1)
	;
		Hp = H
	),
	findAndReplace(Needle, NewNeedle, T, ResP),
	Result = [Hp|ResP]
	.
findAndReplace(N, NN, X, R) :-
	\+ isList(X),
	findAndReplace(N, NN, [X], R).

betaReduction(Func, [], Func) :- !.
betaReduction(Func, [(From, To)|R], Res) :-
	findAndReplace(From, To, Func, FuncP),
	betaReduction(FuncP, R, Res), !.
betaReduction(Func, [_|R], Res) :- betaReduction(Func, R, Res).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Wyrażenia arytmetyczne
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
aexp(Op, L, R, Env, Result) :- 
	% unpack by móc obsłużyć bez problemu kod typu 1+(((1)))
	unpack(L, Lp),
	unpack(R, Rp),
	runCode(Lp, Env, R1p, IE1),
	( IE1 = true ->
		runCode(Rp, Env, R2p, IE)
	;
		R2p = Rp,
		IE = false
	),

	( IE = true, num(R1) = R1p, num(R2) = R2p ->
		( 	Op = 'plus' ,
			ResultP is R1 + R2,
			Result = num(ResultP)
		;
			Op = 'minus' ,
			ResultP is R1 - R2,
			Result = num(ResultP)
		;
			Op = 'times' ,
			ResultP is R1 * R2,
			Result = num(ResultP)
		;
			Op = 'div' ,
			ResultP is R1 div R2,
			Result = num(ResultP)
		;
			Op = 'mod' ,
			ResultP is R1 mod R2,
			Result = num(ResultP)
		;
			Op = '<' ,
			( R1 < R2 ->
				Result = bool(true)
			;
				Result = bool(false)
			)
		;
			Op = '<=' ,
			( R1 =< R2 ->
				Result = bool(true)
			;
				Result = bool(false)
			)
		;
			Op = '>' ,
			( R1 > R2 ->
				Result = bool(true)
			;
				Result = bool(false)
			)
		;
			Op = '>=' ,
			( R1 >= R2 ->
				Result = bool(true)
			;
				Result = bool(false)
			)
		;
			Op = '/=' ,
			( R1 \= R2 ->
				Result = bool(true)
			;
				Result = bool(false)
			)
		;
			Op = '=',
			( R1 = R2 ->
				Result = bool(true)
			;
				Result = bool(false)
			)
		)
	;
		Result = math(Op, R1p, R2p)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Ewaluator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
runMany([], _, [], true).
runMany([H|T], Env, ResP, IE) :-
	runCode(H, Env, R1, IE1),
	( IE1 = true ->
		runMany(T, Env, R2, IE)
	;
		R2 = T,
		IE = false
	),
	ResP = [R1|R2].

execute(fnc(_, []), _, []) :- !.

execute(fnc(_, [PackedBody|RestOfBody]), Env, Result) :-
	unpack(PackedBody, Body),
	% Znów unpack
	% A tu sprawdzamy co mamy w korzeniu drzewa obliczenia
	( 	Body = num(X),
		runCode(Body, _, Result, _), !

	; 	Body = math(Op, Left, Right),
		aexp(Op, Left, Right, Env, Result), !

	; Body = constr(X),
		runMany(RestOfBody, Env, ResP, IsEnd),
		(IsEnd = true ->
			Result = result([constr(X)|ResP])
		;
			Result = [constr(X)|ResP]
		), !

	; Body = [constr(X)|Parms],
		runMany(Parms, Env, ResP, IsEnd),
		(IsEnd = true ->
			Result = result([constr(X)|ResP])
		;
			Result = [constr(X)|ResP]
		), !

	; Body = lambda(Var, Instr),
		(
			RestOfBody = [Arg|RestOfArgs]
		;
			Arg = [], RestOfArgs = []
		),
		betaReduction(Instr, [(Var, Arg)], InstrP),
		unpack(InstrP, InstrPB),
		runCode([InstrPB|RestOfArgs], Env, Res, IsEnd),
		(IsEnd = true ->
			Result = result(Res)
		;
			Result = lambda([], Res)
		), !
	; integer(Body),
		runCode(num(Body), _, Result, _),
		!

	; 	% czyli to bedzie funkcja
		runCode([PackedBody|RestOfBody],  Env, Result, _) 
	), !.

% Różne proste przypadki, które może nam parser wygenerować
% Wyczyszczenie
runCode([X|[]], Env, Res, IE) :- (X = [_|_]; X = result(_); X = constr(_); X = num(_); X = bool(_); X = math(_,_,_)), runCode(X, Env, Res, IE), !.

% Specjalny przypadek, który informuje o zakończeniu obliczeń
runCode(result(X), _, X, true):- !.

runCode([constr(X)|Params], Env, Res, false) :- execute(fnc(_, [constr(X)|Params]), Env, Res), !. 
runCode(constr(X), _, result(constr(X)), false) :- !.
runCode(num(X), _, result(num(X)), false) :- !.
runCode(bool(X), _, result(bool(X)), false) :- !.
runCode(math(Op, L, R), Env, Result, false) :- aexp(Op, L, R, Env, Result), !. 

runCode([lambda(X, Y)|Params], Env, Result, false) :-
	execute(fnc(_, [lambda(X, Y)|Params]), Env, Result), !.
runCode(lambda(X, Y), Env, Result, false) :-
	execute(fnc(_, [lambda(X, Y)]), Env, Result), !.

runCode(Func, Env, Result, false) :-
	% Ciężko mi to było sprowadzić do następnego przypadku dlatego jest prawie zdublowany kod
	\+ isList(Func),
	( findFuncAndMatchPattern(Func, [], Env, Env, patterned(Funct, Reduction)) ->
		Funct = fnc(P, B),
		betaReduction(B, Reduction, Bp),
		execute(fnc(P, Bp), Env, Result), !
	;
		er(Func),
		fail,
		Result = Func	
	)
	.
runCode([Func|Rest], Env, Result, false) :-
	( findFuncAndMatchPattern(Func, Rest, Env, Env, patterned(Funct, Reduction)) ->
		Funct = fnc(P, B),
		betaReduction(B, Reduction, Bp),
		execute(fnc(P, Bp), Env, Result), !
	;
		er(Func),
		fail,
		Result = [Func|Rest]	
	)
	.
runCode(X, _, X, true) :- fail.

er(_). % Predykat do którego warto podłączyć się debuggerem by wiedzieć, ze coś poszło nie tak

runWholeCode(Code, Env, Result) :-
	runCode(Code, Env, R, IE),
	( IE = true ->
		R = Result
	;
		runWholeCode(R, Env, Result)
	).

runCodeWithRemembering(Code, Env, Result) :-
	runWholeCode(Code, Env, Result),
	asserta(runCodeWithRemembering(Code, Env, Result)). % A tutaj mamy małe spamiętywanie

loadFromFile(Filename, OldEnv, Env) :-
	( exists_file(Filename) -> 
		( parseFromFile(Filename, Parsed) ->
			removeDuplicatedFunctions(Parsed, OldEnv, NewEnv),
			loadEnv(Parsed, NewEnv, Env)
		;
			error('Parse error'),
			Env = []
		)
	;
		error('File does not exist'),
		Env = []
	)
	.

execute(Stm, Env) :-
	( parseStm(Stm, Parsed) ->
		( runCodeWithRemembering(Parsed, Env, Result) ->
			prettyWrite(Result)
		;
            prettyWrite(Parsed),
			error('Runtime error')
		)
	;
		error('Parse error')
	).
