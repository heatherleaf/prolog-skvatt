
:- module(utils, [%% meta predicates
		  foreach/2, 
		  countall/2,
		  or_else/2,
		  time/2,
		  %% limited repetition
		  fromto/3,
		  repeat/1, 
		  %% variable instantiation
		  instantiate_variables/2,
		  remove_singles/2,
		  %% randoms
		  randomize/0,
		  shuffle/2,
		  random_goal/1,
		  %% list handling
		  eq_member/2,
		  zip/3,
		  unzip/3
		 ]).

%% sicstus declaration
:- op(950, xfy, or_else).

%% SWI declaration
%% :- op(950, xfy, user:or_else).

:- use_module(library(lists), [member/2, nth0/3]).
:- use_module(library(terms), [term_variables/2]).
:- use_module(library(random), [setrand/1, random/3, randseq/3]).
:- use_module(library(system), [datime/1]).


%% sicstus declaration
:- meta_predicate foreach(:, :), countall(:, ?), or_else(:, :), time(?, :), random_goal(:).

%% SWI declaration
%% :- module_transparent foreach/2, countall/2, or_else/2, time/2, random_goal/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% meta predicates

%% foreach
foreach(Cond, Goal) :- 
	\+ ( Cond, once(Goal), fail ).

%% countall, sicstus version
countall(Goal, N) :-
	bb_put(count, 0),
	foreach(Goal, (bb_get(count,N), N1 is N+1, bb_put(count,N1))),
	bb_get(count, N).

/*
%% countall, SWI version
countall(Goal, N) :-
	flag(count, Old, 0),
	foreach(Goal, flag(count, N, N+1)),
	flag(count, N, Old).
*/

%% or_else
(Goal or_else Handler) :- Goal -> true ; Handler.

%% timing
time(Time, Goal) :- 	
	statistics(runtime, _),
	Goal,
	statistics(runtime, [_, Time]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% limited repetitions

%% fromto/3
fromto(From, To, N) :-
	From =< To,
	(   N = From
	;
	    From1 is From+1,
	    fromto(From1, To, N)
	).

%% repeat/1
repeat(N) :-
	N > 0,
	( true  ;  N1 is N-1, repeat(N1) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% variable instatiation, etc.

instantiate_variables(Term, Used) :-
	number_varlist(Used, 1),
	term_variables(Term, Vars),
	nonumber_vars(Vars).

number_varlist([], _).
number_varlist([X|Xs], N0) :-
	var(X),
	!, N is N0+1,
	X = {N0},
	number_varlist(Xs, N).
number_varlist([_|Xs], N) :-
	number_varlist(Xs, N).

nonumber_vars([]).
nonumber_vars(['_'|Xs]) :-
	nonumber_vars(Xs).

remove_singles(Xs, Ys) :-
	remove_singles(Xs, [], Ys).

remove_singles([], Ys, Ys).
remove_singles([X|Xs], Ys0, Ys) :-
       (   eq_member(X, Xs),
           \+ eq_member(X, Ys0)
       ->  remove_singles(Xs, [X|Ys0], Ys)
       ;   remove_singles(Xs, Ys0, Ys)
       ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% randoms

randomize :-
	datime(datime(Yr,Mn,Dy,Hr,Mi,Sd)),
	X is Yr*Mn+1, Y is Dy*Hr+1, Z is Mi*Sd+1,
	setrand(rand(X,Y,Z)).

shuffle(Xs, Rs) :-
	length(Xs, N),
	randseq(N, N, Ns),
	zip(Ns, Xs, NXs),
	keysort(NXs, NRs),
	unzip(NRs, _, Rs).

%% new version of 24/4-05
random_goal(Goal) :-
	bagof(Goal, Goal, Goals),
	length(Goals, N),
	random(0, N, K),
	nth0(K, Goals, Goal).

/*
random_goal(Goal) :-
	bagof(Goal, Goal, Goals),
	( Goals = [Goal] -> true
	;
	    shuffle(Goals, Shuffled),
	    member(Goal, Shuffled)
	).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% list handling

eq_member(X, L) :-
	member(Y, L), X==Y.

zip([], [], []).
zip([X|Xs], [Y|Ys], [X-Y|XYs]) :-
	zip(Xs, Ys, XYs).

unzip([], [], []).
unzip([X-Y|XYs], [X|Xs], [Y|Ys]) :-
	unzip(XYs, Xs, Ys).


