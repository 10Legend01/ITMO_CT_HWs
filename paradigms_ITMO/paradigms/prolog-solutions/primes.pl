loop(I, S, N) :- 
	I =< N,
	assert(is_composite(I)),
	NEXT_I is I + S, loop(NEXT_I, S, N).
construct_sieve(I, N) :- 
	not is_composite(I),
	NEXT_I is I * I, loop(NEXT_I, I, N).
construct_sieve(I, N) :- 
	I * I =< N,
	NEXT_I is I + 1, construct_sieve(NEXT_I, N).

init(MAX_N) :- construct_sieve(2, MAX_N).
composite(X) :- is_composite(X).
prime(X) :- X > 1, not is_composite(X).

search_min_div(N, I, R) :- 
	I * I < N + 1,
	prime(I),
	0 =:= N mod I, !, R is I.
search_min_div(N, I, R) :- 
	I * I < N + 1, !,
	NEXT_I is I + 1, search_min_div(N, NEXT_I, R).
search_min_div(N, I, R) :- R is N.

calc_list(1, [], _):- !.
calc_list(N, ANS, MIN_DIV):- 
	search_min_div(N, MIN_DIV, H),
	R is N / H,
	calc_list(R, ANS1, H),
	ANS = [H | ANS1].

calc_number(N, [], _, N):- !.
calc_number(N, [H | T], PREV, ANS):- 
	PREV =< H,
	prime(H),
	NEXT is N * H, calc_number(NEXT, T, H, ANS).

prime_divisors(N, L):- integer(N), !, calc_list(N, L, 2).
prime_divisors(N, L):- list(L), !, calc_number(1, L, 1, N).

gcd_dec([H | T1], [H | T2], GCD) :-
	!, gcd_dec(T1, T2, GCD_REC),
	GCD = [H | GCD_REC].
gcd_dec([H1 | T1], [H2 | T2], GCD) :-
	H1 > H2, !,
	gcd_dec([H1 | T1], T2, GCD).
gcd_dec([H1 | T1], [H2 | T2], GCD) :-
	H1 < H2, !,
	gcd_dec(T1, [H2 | T2], GCD).
gcd_dec(_, _, []) :- !.

gcd(A, B, GCD) :- 
	prime_divisors(A, L1),
	prime_divisors(B, L2),
	gcd_dec(L1, L2, GCD_LIST),
	prime_divisors(GCD, GCD_LIST).
