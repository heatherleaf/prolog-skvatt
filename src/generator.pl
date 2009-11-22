

:- module(generator, [ %% generating a random sequence of words
		       generate_corpus/2,
		       generate_words/2,
		       generate_words/3
		     ]).

:- use_module(grammar, [empty_RHS/1, deriv_RHS/3, terminal/2]).
:- use_module(flags, [flag_value/2]).
:- use_module(utils, [random_goal/1, randomize/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generating random sentences, generate_words/2, generate_words/3

generate_corpus(Number, Term, Sentences) :-
	findall(Sentence, (repeat(Number), generate_words(Term, Sentence)), Sentences).

generate_words(Term, Sentence) :-
	flag_value(gendepth, Depth),
	generate_words(Depth, Term, Sentence).

generate_words(Depth, Term, Sentence) :-
	repeat, %% added 24/4-05
	once(gen_words(Depth, Term, Sentence, [])),
	!. %% added 24/4-05

gen_words(Depth, LHS) -->
	{ Depth > 0,
	  NextDepth is Depth-1,
	  random_goal(user:(LHS ---> RHS)) },
	!, %% added 24/4-05
	gen_RHS(RHS, NextDepth).

gen_RHS(RHS, Depth) -->
	{ random_goal(empty_RHS(RHS)) }
	;
	{ random_goal(deriv_RHS(RHS, Term, Rest)) },
	( { nonvar(Term), terminal(Term, Word) } ->
	    [ Word ]
	;   
	    gen_words(Depth, Term)
	),
	!, %% added 24/4-05
	gen_RHS(Rest, Depth).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is to set the random seed 

:- randomize.

