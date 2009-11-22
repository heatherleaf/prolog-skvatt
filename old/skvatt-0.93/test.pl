

ts(A) :-
	atom_length(A, N),
	write(N), nl,
	atom_concat(A, xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx, B),
	ts(B).


