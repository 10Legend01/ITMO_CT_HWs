get_depth(null, 0) :- !.
get_depth(node(Key, Value, Depth, Left, Right), Depth).

max(A, B, A) :- A >= B.
max(A, B, B) :- A < B.
node(Key, Value, Left, Right, node(Key, Value, Depth, Left, Right)) :- 
	get_depth(Left, DL),
	get_depth(Right, DR),
	max(DL, DR, D),
	Depth is D + 1.

map_get(node(K, V, D, L, R), K, V) :- !.
map_get(node(K, V, D, L, R), Key, Value) :- 
	Key < K, !,
	map_get(L, Key, Value).
map_get(node(K, V, D, L, R), Key, Value) :- 
	map_get(R, Key, Value).

balance(node(K, V, D, L, node(K1, V1, D1, L1, R1)), Result) :- % Check RR, L - L1 - R1
	get_depth(L, DL), get_depth(R1, DR),
	D1 - DL =:= 2, D1 - DR =:= 1, !,
	node(K, V, L, L1, NewL),
	node(K1, V1, NewL, R1, Result).
balance(node(K, V, D, node(K1, V1, D1, L1, R1), R), Result) :- % Check LL, L1 - R1 - R
	get_depth(R, DR), get_depth(L1, DL),
	D1 - DR =:= 2, D1 - DL =:= 1, !,
	node(K, V, R1, R, NewR),
	node(K1, V1, L1, NewR, Result).
balance(node(K, V, D, L, node(K1, V1, D1, node(K2, V2, D2, L2, R2), R1)), Result) :- % Check RL, L - L2 - R2 - R1
	get_depth(L, DL), get_depth(R1, DR),
	D1 - DL =:= 2, D1 - DR =:= 2, !,
	node(K, V, L, L2, NewL),
	node(K1, V1, R2, R1, NewR),
	node(K2, V2, NewL, NewR, Result).
balance(node(K, V, D, node(K1, V1, D1, L1, node(K2, V2, D2, L2, R2)), R), Result) :- % Check LR, L1 - L2 - R2 - R
	get_depth(R, DR), get_depth(L1, DL),
	D1 - DR =:= 2, D1 - DL =:= 2, !,
	node(K1, V1, L1, L2, NewL),
	node(K, V, R2, R, NewR),
	node(K2, V2, NewL, NewR, Result).
balance(Result, Result) :- !.

del_left(node(K, V, D, null, R), K, V, R) :- !.
del_left(node(K, V, D, L, R), NewK, NewV, Result) :-
	del_left(L, NewK, NewV, NewL),
	node(K, V, NewL, R, Result1),
	balance(Result1, Result).

del(node(K, V, D, L, null), K, L) :- !.
del(node(K, V, D, null, R), K, R) :- !.
del(node(K, V, D, L, R), K, Result) :-
	del_left(R, NewK, NewV, NewR),
	node(NewK, NewV, L, NewR, Result1),
	balance(Result1, Result).
del(node(K, V, D, L, R), Key, Result) :-
	Key < K, !,
	del(L, Key, Result1),
	node(K, V, Result1, R, Result2),
	balance(Result2, Result).
del(node(K, V, D, L, R), Key, Result) :-
	del(R, Key, Result1),
	node(K, V, L, Result1, Result2),
	balance(Result2, Result).

map_remove(TreeMap, Key, Result) :-
	map_get(TreeMap, Key, Value), !,
	del(TreeMap, Key, Result).
map_remove(Result, _, Result) :- !.

map_put(null, Key, Value, Result) :- !, node(Key, Value, null, null, Result).
map_put(node(K, V, D, L, R), K, Value, Result) :- !, node(K, Value, L, R, Result).
map_put(node(K, V, D, L, R), Key, Value, Result) :- 
	Key < K, !,
	map_put(L, Key, Value, Result1),
	node(K, V, Result1, R, Result2),
	balance(Result2, Result).
map_put(node(K, V, D, L, R), Key, Value, Result) :- 
	K < Key, !,
	map_put(R, Key, Value, Result1),
	node(K, V, L, Result1, Result2),
	balance(Result2, Result).

map_build([], null) :- !.
map_build([(K, V) | L], TreeMap) :- 
	map_build(L, T),
	map_put(T, K, V, TreeMap).

map_getLast(node(K, V, D, L, null), (K, V)) :- !.
map_getLast(node(K, V, D, L, R), (Key, Value)) :-
	map_getLast(R, (Key, Value)).

map_removeLast(node(K, V, D, L, null), L) :- !.
map_removeLast(node(K, V, D, L, R), Result) :- !,
	map_removeLast(R, NewR),
	node(K, V, L, NewR, Result1),
	balance(Result1, Result).
map_removeLast(Result, Result) :- !.
