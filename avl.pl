halfs(L, H1, H2) :-
   length(L, Length),
   Length1 is div(Length, 2),
   Length2 is Length - Length1,
   length(H1, Length1),
   length(H2, Length2),
   append(H1, H2, L).

fill_nil(nil, V, V) :- !.
fill_nil(O, V, Output) :- O \= nil, !, Output = O.

max(X, Y, O) :- X < Y, !, O is Y.
max(X, Y, O) :- X >= Y, !, O is X.

less(X, Y) :- number(X), number(Y), X < Y, !.
eq(X, Y) :- number(X), number(Y), X == Y, !.

ordered([]) :- !.
ordered([H]) :- !.
ordered([H1, H2 | T]) :-
	less(H1, H2), !,
	ordered([H2 | T]), !.
ordered([H1, H2 | T]) :-
	eq(H1, H2), !,
	ordered([H2 | T]), !.

value(nil, nil) :- !.
value(tree(V, _, _, _), V) :- !.

height(nil, 0) :- !.
height(tree(_, Height, _, _), Height) :- !.

left(nil, nil) :- !.
left(tree(_, _, L, _), L) :- !.

right(nil, nil) :- !.
right(tree(_, _, _, R), R) :- !.

update_node(tree(P, _, L, R), tree(P, NewHeight, L, R)) :-
	height(L, LeftHeight),
	height(R, RightHeight),
	max(LeftHeight, RightHeight, Height),
	NewHeight is Height + 1.

calc_balance(nil, 0) :- !.
calc_balance(tree(_, _, L, R), Balance) :-
	height(L, LeftHeight),
	height(R, RightHeight),
	Balance is LeftHeight - RightHeight.


%       A           B
%      / \         / \
%     B   C  ==>  D   A
%    / \             / \
%   D   E           E   C
rotate_right(tree(A, _, tree(B, _, D, E), C), O) :-
	update_node(tree(A, 0, E, C), Right),
	update_node(tree(B, 0, D, Right), O).

%       A           C
%      / \         / \
%     B   C  ==>  A   E
%        / \     / \
%       D   E   B   D
rotate_left(tree(A, _, B, tree(C, _, D, E)), O) :-
	update_node(tree(A, 0, B, D), Left),
	update_node(tree(C, 0, Left, E), O).

balance(nil, nil) :- !.

balance(T, O) :-
	calc_balance(T, Balance),
	AbsBalance is abs(Balance),
	AbsBalance =< 1, !,
	O = T.

balance(T, Output) :-
	left(T, Left),
	right(T, Right),
	value(T, Value),
	calc_balance(T, Balance),
	calc_balance(Left, LBalance),
	2 is Balance,
	LBalance < 0, !,
	rotate_left(Left, NewLeft),
  update_node(tree(Value, 0, NewLeft, Right), RawOutput),
	rotate_right(RawOutput, Output).

balance(T, Output) :-
	right(T, Right),
	calc_balance(T, Balance),
	calc_balance(Left, LBalance),
	2 is Balance,
	LBalance >= 0, !,
	rotate_right(T, Output).

balance(T, Output) :-
	left(T, Left),
	right(T, Right),
	value(T, Value),
	calc_balance(T, Balance),
	calc_balance(Right, RBalance),
	-2 is Balance,
	RBalance > 0, !,
	rotate_right(Right, NewRight),
  update_node(tree(Value, 0, Left, NewRight), RawOutput),
	rotate_left(RawOutput, Output).

balance(T, Output) :-
	left(T, Left),
	calc_balance(T, Balance),
	calc_balance(Right, RBalance),
	-2 is Balance,
	RBalance =< 0, !,
	rotate_left(T, Output).

insert(nil, X, tree(X, 1, nil, nil)) :- !.
insert(tree(Value, Height, Left, Right), X, Output) :-
	eq(X, Value), !,
	Output = tree(X, Height, Left, Right).

insert(T, X, O) :-
	value(T, Value),
	\+ less(X, Value), !,
	left(T, Left),
	right(T, Right),
	insert(Right, X, NewRight),
	update_node(tree(Value, 0, Left, NewRight), Output),
	balance(Output, O).

insert(T, X, O) :-
	value(T, Value),
	less(X, Value), !,
	left(T, Left),
	right(T, Right),
	insert(Left, X, NewLeft),
	update_node(tree(Value, 0, NewLeft, Right), Output),
	balance(Output, O).

remove_min(nil, _, nil) :- !.

remove_min(tree(Value, _, Left, Right), Min, Updated) :-
	nil == Left, !,
	Min = Value,
	Updated = Right.

remove_min(tree(Value, _, Left, Right), Min, Updated) :-
	Left \= nil, !,
	remove_min(Left, Min, NewLeft),
	update_node(tree(Value, 0, NewLeft, Right), Updated).

remove(nil, _, nil) :- !.

remove(tree(Value, _, Left, Right), X, Output) :-
	eq(X, Value),
	Right == nil, !,
	Output = Left.

remove(tree(Value, _, Left, Right), X, Output) :-
	eq(X, Value), !,
	Right \= nil, !,
	remove_min(Right, RightMin, NewRight),
	update_node(tree(RightMin, 0, Left, NewRight), Output).

remove(tree(Value, _, Left, Right), X, Output) :-
	less(X, Value), !,
	remove(Left, X, NewLeft),
	update_node(tree(Value, 0, NewLeft, Right), Output).

remove(tree(Value, _, Left, Right), X, Output) :-
	\+ less(X, Value),
	\+ eq(X, Value), !,
	remove(Right, X, NewRight),
	update_node(tree(Value, 0, Left, NewRight), Output).

search(tree(Value, _, _, _), X, Output) :-
	eq(X, Value), !,
	Output = Value.

search(tree(Value, _, Left, _), X, Output) :-
	less(X, Value), !,
	search(Left, X, Output).

search(tree(Value, _, _, Right), X, Output) :-
	\+ eq(X, Value),
	\+ less(X, Value), !,
	search(Right, X, Output).

floor_key(nil, X, nil) :- !.

floor_key(tree(Value, _, _, _), X, Output) :-
	eq(Value, X), !,
	Output = Value.

floor_key(tree(Value, _, _, Right), X, Output) :-
	less(Value, X), !,
	floor_key(Right, X, O1),
	fill_nil(O1, Value, Output).

floor_key(tree(Value, _, Left, _), X, Output) :-
	\+ eq(Value, X),
	\+ less(Value, X), !,
	floor_key(Left, X, Output).

fill(T, [], T).

fill(Tree, [H | T], Output) :-
	insert(Tree, H, NewTree),
	fill(NewTree, T, Output).

sorted_list_to_tree([], nil) :- !. 
sorted_list_to_tree([Value], tree(Value, 1, nil, nil)) :- !. 
sorted_list_to_tree(SortedList, Tree) :-
	% H2 is guaranteed to be >= H1
	halfs(SortedList, H1, [RootValue | H2]),
	sorted_list_to_tree(H1, Left),
	sorted_list_to_tree(H2, Right),
	update_node(tree(RootValue, 0, Left, Right), Tree). 

build(Items, Output) :- 
	\+ ordered(Items), !,
	fill(nil, Items, Output).

build(Items, Output) :- 
	ordered(Items), !,
	sorted_list_to_tree(Items, Output).

% Map Functions
entry_list([], []) :- !.
entry_list([(Key, Value) | T], Output) :-
	entry_list(T, O1),
	Output = [entry(Key, Value) | O1].

eq(entry(K1, _), entry(K1, _)).
less(entry(K1, _), entry(K2, _)) :- K1 @< K2.

map_build(ListMap, TreeMap) :-
	entry_list(ListMap, Entries),
	build(Entries, TreeMap), !.

map_get(TreeMap, Key, Value) :- search(TreeMap, entry(Key, nil), entry(_, Value)).

map_put(TreeMap, Key, Value, Result) :- insert(TreeMap, entry(Key, Value), Result), !.

map_remove(TreeMap, Key, Result) :- remove(TreeMap, entry(Key, nil), Result).

map_floorKey(Map, Key, FloorKey) :- floor_key(Map, entry(Key, nil), entry(FloorKey, _)).
