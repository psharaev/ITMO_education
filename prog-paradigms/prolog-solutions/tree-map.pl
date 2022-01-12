head([H | T], H).
tail([H | T], T).
getNodeData((K, V), K, V).

map_build(ListMap, TreeMap) :- length(ListMap, Len), rb(Len, ListMap, [], TreeMap).
%rb(Length, Array, TailRest, Root)
rb(0, Arr, Arr, null) :- !.
rb(1, [(K, V) | T], T, node(K, V, null, null)) :- !.
rb(Len, Arr, Tail, node(K, V, LC, RC)) :- M is div(Len, 2),
	rb(M, Arr, Untreated, LC), % get left child
	head(Untreated, Root), getNodeData(Root, K, V), % get root
	tail(Untreated, RightChildItems), RightLen is (Len - M - 1), rb(RightLen, RightChildItems, Tail, RC). % get right child

map_get(node(K, V, _, _), K, V).
map_get(node(X, _, L, _), K, V) :- K < X, map_get(L, K, V).
map_get(node(X, _, _, R), K, V) :- K > X, map_get(R, K, V).

map_keys(null, []) :- !.
map_keys(node(K, V, null, null), [K]) :- !.
map_keys(node(K, V, LC, RC), Res) :- map_keys(LC, LRes), append(LRes, [K], MRes), map_keys(RC, RRes), append(MRes, RRes, Res).

map_values(null, []) :- !.
map_values(node(K, V, null, null), [V]) :- !.
map_values(node(K, V, LC, RC), Res) :- map_values(LC, LRes), append(LRes, [V], MRes), map_values(RC, RRes), append(MRes, RRes, Res).