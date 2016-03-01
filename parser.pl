% SWI-Prolog

:- module(parser, [parse/2, parseStm/2, parseFromFile/2]).

/*
   Analiza leksykalna oparta na przykładzie z wykładu
*/ 

lexer(Tokens) -->
   white_space,
   (  (  ";",       !, { Token = tokColon }
      ;  "(",       !, { Token = tokLParen }
      ;  ")",       !, { Token = tokRParen }
	  ;  "{",       !, { Token = tokLMusta}
	  ;  "}",       !, { Token = tokRMusta}
	  ;  "\\",      !, { Token = tokSlash}
	  ;  "->",      !, { Token = tokImpl}
	  ;  "+",       !, { Token = tokPlus }
	  ;  "-",       !, { Token = tokMinus }
	  ;  "*",       !, { Token = tokTimes }
	  ;  "/=",      !, { Token = tokNEq }
	  ;  "=",       !, { Token = tokEq }
	  ;  "<=",      !, { Token = tokLeq }
	  ;  "<",       !, { Token = tokLt }
	  ;  ">=",      !, { Token = tokGeq }
	  ;  ">",       !, { Token = tokGt }
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (div, tokDiv),
                                     (mod, tokMod),
                                     (where, tokWhere)]),
               !
            ;  Token = tokVar(Id)
            }
      ;  [_],
            { Token = tokUnknown }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).



white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
% Reguła odpowiedzialna za usuwanie komentarzy
white_space -->
	"--", whole_line, !, white_space.
white_space -->
   [].

whole_line --> "\n", !.
whole_line --> [_], whole_line.
   
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { code_type(A, alnum) }, !, alphanum(T).
alphanum([]) -->
   [].

alphanum([]).
alphanum([H|T]) :- code_type(H, alpha), alphanum(T).

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.



/*
* Parser
* W zasadzie przepisany prawie bezpośrednio z gramatyki, plus wpisanie priorytetów i usunięcie lewostronnej rekursji,
* dodatkowo parser oznacza specjalnymi funktorami liczby i stałe logiczne.
*/

program(Ast) -->
	lclause(Cl), !, program(Rest), !,
	{Ast = [Cl| Rest]}.
program(Ast) --> [], {Ast = []}.

clauses(Cl) --> [tokLMusta], !,  program(Ast),[tokRMusta], {Cl = (Ast)}.
clauses(Cl) --> lclause(Cl).

lclause(Cl) --> clause_hed((Head, Params)),  [tokEq], !, expr(Phr), local_decl(LD),
{Cl = func(Head, Params, Phr, LD)}.

local_decl(LD) --> [tokColon], !, {LD = []}.
local_decl(LD) --> [tokWhere], !, clauses(Cl), {LD = Cl}.

clause_hed(Head) --> [tokVar(X)],!, params(Par), {Head = (X, Par)}.
params(Par) --> param(X), params(Par2), { Par = [X|Par2] }.
params(Par) --> [], {Par = []}.

param(X) --> constructor(X), !.
param(X) --> variable(X), !.
param(X) --> [tokLParen], !,  pattern(X), [tokRParen].

pattern(X) --> constructor(Con), params(Par), {X = (Con, Par)}.
pattern(X) --> [tokNum(Z)], {X = num(Z)}, !.
pattern(X) --> [tokVar(X)], !.

expr(Expr) --> [tokSlash], !, [tokVar(X)], [tokImpl], expr(E2), {Expr = lambda(X, E2)}.
expr(Expr) --> simple_expr(Expr).

/* Operatory */
op_g3(Op) --> ([tokTimes], {Op = 'times'}; [tokDiv], {Op = 'div'}; [tokMod], {Op = 'mod'}).
op_g2(Op) --> ([tokPlus], {Op = 'plus'}; [tokMinus], {Op = 'minus'}).
op_g1(Op) --> ([tokLt], {Op = '<'}; [tokGt], {Op = '>'}; [tokLeq], {Op = '<='}; [tokGeq], {Op = '>='}; [tokEq], {Op = '='}; [tokNEq], {Op = '/='}).


/* Metoda usuwania lewostronnej rekursji przedstawiona na wykładzie */
simple_expr(Expr) --> sexp1(A), simple_expr(A, Expr).

simple_expr(A, Expr) --> op_g1(Op), !, sexp1(E2), {A1 = [math(Op, A, E2)]}, simple_expr(A1, Expr).
simple_expr(A, A) --> "".

sexp1(Expr) --> sexp2(A), sexp1(A, Expr).

sexp1(A, Expr) --> op_g2(Op), !, sexp2(E2), {A1 = [math(Op, A, E2)]}, sexp1(A1, Expr).
sexp1(A, A) --> "".

sexp2(Expr) --> expr_application(E1), op_g3(Op), !, expr_application(E2), {Expr = [math(Op, E1, E2)]}.
sexp2(Expr) --> expr_application(Expr), !.

expr_application(Expr) --> atom_expr(E1), expr_application(E2), !,   {Expr = [E1|E2]}.
expr_application(Expr) --> atom_expr(ExprP), { Expr = [ExprP] }.

atom_expr(Expr) --> [tokLParen],  expr(E1), [tokRParen], !, { Expr = (E1) }.
atom_expr(Expr) --> [tokNumber(X)], !, {Expr = num(X)}.
atom_expr(Expr) --> constructor(Expr1), {Expr = Expr1}.
atom_expr(Expr) --> [tokVar(X)], !, {Expr = X}.

constructor(L) --> [tokVar(Name)], {name(Name, [H|_]), code_type(H, upper), 
	( Name = 'True' ->
		L = bool(true)
	;
		(Name = 'False' ->
		L = bool(false)
		;
		L = constr(Name)
		)
	)

	}.
constructor(L) --> [tokNumber(LP)], {L = num(LP)}.

variable(L) --> [tokVar(Name)], {name(Name, [H|_])},{code_type(H, lower), L = variable(Name)}.


parse(CharCodeList, Absynt) :-
   phrase(lexer(TokList), CharCodeList),
   phrase(program(Absynt), TokList).

parseFromFile(Filename, Absynt) :-
   phrase_from_file(lexer(TokList), Filename),
   phrase(program(Absynt), TokList).

parseStm(CharCodeList, Absynt) :-
   phrase(lexer(TokList), CharCodeList),
   phrase(expr(Absynt), TokList).
