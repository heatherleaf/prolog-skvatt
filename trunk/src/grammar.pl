

:- module(grammar, [ %% pretty-printing the grammar
		     grammar/0,
		     %% translating an RHS term to a terminal
		     terminal/2,
		     %% derivating an RHS
		     deriv_RHS/3,
		     empty_RHS/1
		   ]).


:- op(1200, xfx, --->).
:- op(1199, xfx, when).
:- op(1198, xfx, where).

:- use_module(library(lists), [is_list/1]).
:- use_module(utils, [foreach/2]).
:- use_module(errors, [error/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checking grammar productions, term_expansion/2

:- multifile user:term_expansion/2.
:- dynamic   user:term_expansion/2.

user:term_expansion(Prod, _) :-
	\+ prolog_load_context(module, grammar),
	nonvar(Prod), Prod = (LHS ---> RHS),
	\+ check_production(LHS, RHS, Prod),
	error(production(Prod)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% terminals: translation between terms and words

%% terminal(?terminal, ?word)
terminal('$word'(Word), Word).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% operations on RHS's

%% deriv_RHS(+RHS, ?term, ?rest of RHS)
deriv_RHS(X, X, []) :-
	var(X), !.
deriv_RHS([Word|Rest], Term, Rest) :-
	!, terminal(Term, Word).
deriv_RHS((Left,Right), Next, Rest) :-
	!, deriv_conj(Left, Right, Next, Rest).
deriv_RHS((Left;Right), Next, Rest) :-
	!, ( deriv_RHS(Left, Next, Rest)
	   ; deriv_RHS(Right, Next, Rest)
	   ).
deriv_RHS({_}, _, _) :-
	!, fail.
deriv_RHS((RHS when Goal), Next, (Rest when Goal)) :-
	!, deriv_RHS(RHS, Next, Rest).
deriv_RHS((RHS where Constraints), Next, Rest) :-
	!, user:Constraints, deriv_RHS(RHS, Next, Rest).
deriv_RHS(Next, Next, []).

%% deriv_conj(+RHS1, +RHS2, ?term, ?rest of RHS's)
deriv_conj(X, Rest, X, Rest) :- 
	var(X), !.
deriv_conj([], RHS, Next, Rest) :- 
	!, deriv_RHS(RHS, Next, Rest).
deriv_conj([Word|Words], Rest, Term, (Words,Rest)) :- 
	!, terminal(Term, Word).
deriv_conj((Left,Right), RHS, Next, Rest) :- 
	!, deriv_conj(Left, (Right,RHS), Next, Rest).
deriv_conj((Left;Right), RHS, Next, Rest) :- 
	!, ( deriv_conj(Left, RHS, Next, Rest) 
	   ; deriv_conj(Right, RHS, Next, Rest) ).
deriv_conj({Goal}, RHS, Next, Rest) :-
	!, user:Goal, deriv_RHS(RHS, Next, Rest).
deriv_conj(Next, Rest, Next, Rest).

%% empty_RHS(+RHS)
empty_RHS(X) :- 
	var(X), !, fail.
empty_RHS([]).
empty_RHS((Left,Right)) :-
	empty_RHS(Left), empty_RHS(Right).
empty_RHS((Left;Right)) :-
	empty_RHS(Left) ; empty_RHS(Right).
empty_RHS({Goal}) :-
	user:Goal.
empty_RHS((RHS when Goal)) :-
	empty_RHS(RHS), user:Goal.
empty_RHS((RHS where Constraints)) :-
	user:Constraints, empty_RHS(RHS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checking a grammar production, check_production/3

check_production(LHS, RHS0, Prod) :-
	check_LHS(LHS, Prod),
	split_RHS(RHS0, RHS, Constraints, Goal),
	check_RHS(RHS, Prod),
	check_constraints(Constraints, Prod),
	check_goal(Goal, Prod).

check_LHS(LHS, Prod) :- 
	( is_nonterminal(LHS) ->
	    true
	; error(lhs(LHS, Prod)) ).

check_RHS(RHS, Prod) :-
	( var(RHS) -> 
	    true
	; ( RHS = (L,R) ; RHS = (L;R) ) ->
	    check_RHS(L, Prod), 
	    check_RHS(R, Prod)
	; RHS = {Goal} ->
	    check_goal(Goal, Prod)
	; ( is_terminal(RHS) ; is_nonterminal(RHS) ) ->
	    true
	; error(rhs(RHS, Prod)) ).

check_constraints(Cns, Prod) :-
	( nonvar(Cns), ( Cns = (L,R) ; Cns = (L;R) ) ->
	    check_constraints(L, Prod),
	    check_constraints(R, Prod)
	; is_constraint(Cns) -> 
	    true
	; error(constraints(Cns, Prod)) ).

check_goal(Goal, Prod) :-
	( is_declarative(Goal) ->
	    error(goal(Goal, Prod))
	; true ).

is_constraint(Constr) :-
	nonvar(Constr), 
	(
	  Constr = true
	;
	  Constr = (A=B), 
	  is_nonterminal(A), 
	  is_nonterminal(B)
	).

is_terminal(T) :-
	is_list(T).

is_nonterminal(NT) :- 
	( 
	  var(NT)
	;
	  \+ is_goal(NT), 
	  \+ is_declarative(NT)
	).

is_goal((_, _)).
is_goal((_ ; _)).
is_goal((_ -> _)).
is_goal((\+ _)).
is_goal((_ = _)).
is_goal((_ \= _)).
is_goal((_ == _)).
is_goal((_ \== _)).

is_declarative((_ :- _)).
is_declarative((_ --> _)).
is_declarative((_ ---> _)).
is_declarative((_ where _)).
is_declarative((_ when _)).
is_declarative((:- _)).
is_declarative((?- _)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% printing the current grammar, grammar/0

grammar :-
	foreach(( user:(LHS ---> RHS),
		  numbervars(LHS-RHS, 0, _)
		),
		print_production(LHS, RHS)).

print_production(LHS, RHS0) :-
	split_RHS(RHS0, RHS, Constraints, Goal),
	print(LHS), 
	write(' ---> \n\t'),
	print_goal(RHS),
	( Constraints \= true ->
	    write('\n    where\n\t'),
	    print_goal(Constraints)
	; true ),
	( Goal \= true ->
	    write('\n    when\n\t'),
	    print_goal(Goal)
	; true ),
	write('.\n').

print_goal(Goal) :-
	print_goal(Goal, '\t').

print_goal(Goal, Indent) :-
	( Goal = (Left,Right) ->
	    print_goal(Left, Indent),
	    write(',\n'), write(Indent),
	    print_goal(Right, Indent)
	; Goal = (Left;Right) ->
	    write('(   '),
	    atom_concat(Indent, '    ', Indent1),
	    print_goal(Left, Indent1),
	    nl, write(Indent),
	    write(';   '),
	    print_goal(Right, Indent1),
	    nl, write(Indent),
	    write(')')
	; print(Goal) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% splitting an RHS into parts, split_RHS/4

split_RHS((RHS where Where when When), RHS, Where, When) :-
	nonvar(Where), nonvar(When), !.
split_RHS((RHS where Where), RHS, Where, true) :-
	nonvar(Where), !.
split_RHS((RHS when When), RHS, true, When) :-
	nonvar(When), !.
split_RHS(RHS, RHS, true, true).


