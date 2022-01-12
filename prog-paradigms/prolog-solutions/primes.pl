prime(X) :- X > 1, N is div(X, 2), not have_div(X, 2, N).

have_div(X, I, N) :- I =< N, 0 is mod(X, I).
have_div(X, I, N) :- I < N, I1 is I + 1, have_div(X, I1, N).

composite(X) :- not prime(X).

prime_divisors(1, []).
prime_divisors(X, [X]) :- prime(X), !.
prime_divisors(X, [H | T]) :- X > 1, min_div(X, 2, H), N is div(X, H), prime_divisors(N, T).

min_div(X, I, R) :- I * I =< X, prime(I), 0 is mod(X, I), R is I, !.
min_div(X, I, R) :- I * I =< X, N is I + 1, !, min_div(X, N, R).
min_div(X, I, R) :- R is X, !.

power_divisors(X, 0, []).
power_divisors(X, I, D) :- I > 0, pd(X, I, I, D).

pd(1, I, M, []).
pd(X, 1, M, [X]) :- prime(X), !.
pd(X, I, M, [H | T]) :- X > 1, I > 1, min_div(X, 2, H), I1 is I - 1, pd(X, I1, M, T).
pd(X, 1, M, [H | T]) :- X > 1, min_div(X, 2, H), N is div(X, H), pd(N, M, M, T).
