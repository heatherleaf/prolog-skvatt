

:- op(1150, fx, depth).


:- module(generator, [ %% generating a random sequence
		       generate/3,
		       %% setting the maximum search depth
		       depth/1
		     ]).

%% ?- generate(?term, -word list, -tree).
%% ?- depth +integer.

:- use_module(grammar).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generating random sentences, generate/3

generate(Term, Sentence, Tree) :-
	bb_get(generator:depth, Depth),
	generate(Depth, Term, Sentence, Tree).

generate(Depth, Term, Sentence, Tree) :-
	read_grammar,
	repeat,
	generate(Depth, Term, Tree, Sentence, []),
	!.

generate(s(Depth), LHS, node(LHS,Trees)) -->
	{ random_production(LHS, RHS) },
	generate_rhs(RHS, Depth, Trees).

generate_rhs([], _, []) --> [].
generate_rhs([Term|RHS], Depth, OutTrees) -->
	( { nonvar(Term), terminal(Word,Term) } ->
	    [ Word ],
	    { Tree = leaf(Word),
	      OutTrees = [Tree|Trees] }
	; { nonvar(Term), Term = {PrologGoal} } ->
	    { random_goal(PrologGoal),
	      OutTrees = Trees }
	;   
	    generate(Depth, Term, Tree),
	    { OutTrees = [Tree|Trees] }	
	),
	generate_rhs(RHS, Depth, Trees).
	

random_production(LHS, RHS) :-
	random_clause(production(LHS,RHS), true).

random_goal(X) :-
	var(X),
	!, throw(instantiation_error(X,0)).
random_goal((Goal1,Goal2)) :-
	!, random_goal(Goal1), random_goal(Goal2).
random_goal((\+ Goal)) :-
	!, \+ random_goal(Goal).
random_goal((Test -> GoalT ; GoalF)) :-
	!, ( random_goal(Test) -> random_goal(GoalT) ; random_goal(GoalF) ).
random_goal((Test -> GoalT)) :-
	!, ( random_goal(Test) -> random_goal(GoalT) ).
random_goal((Goal1 ; Goal2)) :-
	!, ( random_goal(Goal1) ; random_goal(Goal2) ).
random_goal(!) :-
	!, print_message(warning, 'Cuts (!) will not be correctly executed').
random_goal(Goal) :-
	predicate_property(user:Goal, dynamic),
	!, random_clause(user:Goal, Body),
	random_goal(Body).
random_goal(Goal) :-
	user:Goal.

random_clause(Head, Body) :-
	bagof(Head-Body, clause(Head,Body), Clauses),
	length(Clauses, Num),
	random(0, Num, N),
	nth0(N, Clauses, Head-Body).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the maximum search depth for generation, depth/1

depth(Depth0) :-
	integer(Depth0),
	Depth0 > 0,
	conv_depth(Depth0, Depth),
	bb_put(generator:depth, Depth).

conv_depth(0, zero).
conv_depth(N0, s(D)) :-
	N0 > 0, N is N0-1,
	conv_depth(N, D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is to set the random seed 

:- datime(datime(Yr,Mn,Dy,Hr,Mi,Sd)),
   X is Yr*Mn+1, Y is Dy*Hr+1, Z is Mi*Sd+1,
   setrand(rand(X,Y,Z)).


%% Setting the initial search depth 

:- depth 5.

