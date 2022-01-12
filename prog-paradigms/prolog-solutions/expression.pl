:- load_library('alice.tuprolog.lib.DCGLibrary').
expr_p(variable(Name)) --> [Name], { member(Name, [x, y, z]) }.

nonvar(V, _) :- var(V).
nonvar(V, T) :- nonvar(V), call(T).

digits_p([]) --> [].
digits_p([H | T]) --> 
  { member(H, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', '-'])},
  [H], 
  digits_p(T).

expr_p(const(Value)) --> 
  { nonvar(Value, number_chars(Value, Chars)) },
  digits_p(Chars),
  { Chars = [_ | _], atom_chars(Values, Chars), text_term(Values, Value) }.

op_p(op_add) --> ['+'].
op_p(op_subtract) --> ['-'].
op_p(op_multiply) --> ['*'].
op_p(op_divide) --> ['/'].
op_p(op_negate) --> [n,e,g,a,t,e].
op_p(op_sin) --> [s,i,n].
op_p(op_cos) --> [c,o,s].

expr_p(operation(Op, A, B)) --> ['('], expr_p(A), [' '], expr_p(B), [' '], op_p(Op), [')'].
expr_p(operation(Op, A)) --> ['('], expr_p(A), [' '], op_p(Op), [')'].

suffix_str(E, A) :- ground(E), phrase(expr_p(E), C), atom_chars(A, C), !.

skipSpace([], [], _):-!.
skipSpace([H | T], [H1 | R], 1) :- H = ' ', !, H1 = '!', skipSpace(T, R, 0).
skipSpace([H | T], R, 0) :- H = ' ', !, skipSpace(T, R, 0).
skipSpace([H | T], [H | R], _) :- skipSpace(T, R, 1).

skip([], []) :- !.
skip([')'], [')']) :- !.
skip(['!'], []) :- !.
skip([H1, H2 | T], [H | R]) :- H1 = '!', H2 = ')', H = ')', !, skip(T, R).
skip([H1, H2 | T], [H | R]) :- H1 = '(', H2 = '!', H = '(', !, skip(T, R).
skip([H | T], [H | R]) :- skip(T, R).

finalSkip([], []) :- !.
finalSkip([H | T], [H1 | R]) :- H = '!', !, H1 = ' ', finalSkip(T, R).
finalSkip([H | T], [H | R]) :- finalSkip(T, R).

suffix_str(E, A) :- atom(A), atom_chars(A, C), skipSpace(C, R, 0), skip(R, R1), finalSkip(R1, R2), phrase(expr_p(E), R2).

operation(op_add, A, B, R) :- R is A + B.
operation(op_subtract, A, B, R) :- R is A - B.
operation(op_multiply, A, B, R) :- R is A * B.
operation(op_divide, A, B, R) :- R is A / B.
operation(op_negate, A, R) :- R is -A.
operation(op_sin, A, R) :- R is sin(A).
operation(op_cos, A, R) :- R is cos(A).

lookup(K, [(K, V) | _], V).
lookup(K, [_ | T], V) :- lookup(K, T, V).

evaluate(const(V), _, V).
evaluate(variable(Name), Vars, R) :- lookup(Name, Vars, R).
evaluate(operation(Op, A, B), Vars, R) :-
	evaluate(A, Vars, AR),
	evaluate(B, Vars, BR),
	operation(Op, AR, BR, R).
evaluate(operation(Op, A), Vars, R) :-
	evaluate(A, Vars, AR),
	operation(Op, AR, R).