% Insertion procedure for insertion sort. 
% Inserts based on the sum of the sublists.
i(X, [], [X]) :- !.
i(
	sublist(L1, I1, J1, S1),
	[sublist(L2, I2, J2, S2)|T],
	[sublist(L1, I1, J1, S1),sublist(L2, I2, J2, S2)|T]
	) :-  S1 =< S2, !.
i(X, [X1|T], [X1|T1]) :- i(X, T, T1).

% Insertion sort
i_sort([], []).
i_sort([X|T], T2) :- i_sort(T, T1), i(X, T1, T2).

% Matches a list of sublists to a list L.
% The sublist structures have the form (X, I, J, S), where
%		X: the sublist.
%		I: the (zero-based) starting index of X in list L.
%		J: the (zero-based) ending index of X in list L.
%		S: the sum of all elements in X.
%	Does not return empty sublists.
sub_lists(sublist(X, I, J, S), L) :- append(L1, L2, L), append(X, _, L2),
	length(L1, Len1),
	length(X, Len2), 
	sum_set(X, S),
	Len2 > 0, %Filter out empty sets
	I is Len1,
	J is Len1 + Len2 - 1.

% Compute the sum of a set.
sum_set([], 0).
sum_set([I|T], S) :- sum_set(T, S1), S is I+S1.

% Compute all possible (except for empty) sublists of a list L.
all_sub_lists(L, R) :- findall(X, sub_lists(X, L), R).

% Return only one of all possible sorting orders.
unique_sorted(L, R) :- findall(X, i_sort(L, X), [R|_]).

% Take the N first elements of a list.
take(0, _, []).
take(N, [X|Xs], [X|Ys]) :- N1 is N-1, take(N1, Xs, Ys).

% Compute the K smallest sets of L.
smallest_k_sets(L, K, S) :- 
	all_sub_lists(L, AllSubLists),
	unique_sorted(AllSubLists, Sorted),
	take(K, Sorted, S).

print_set(sublist(X, I, J, S)) :- write(S), tab(4), write(I), tab(4), write(J), tab(4), write(X), nl.

print_sets([]).
print_sets([X|Xs]) :- print_set(X), print_sets(Xs).

print_header() :- write(size), tab(4), write(i), tab(4), write(j), tab(4), write(sublist), nl.

run([], _) :- write("Error: Empty input list").
run(L, K) :- print_header, smallest_k_sets(L, K, Sets) ,print_sets(Sets).
