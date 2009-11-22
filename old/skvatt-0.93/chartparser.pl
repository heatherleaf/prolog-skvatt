

:- module(chartparser, [ %% parsing, returning a tree
			 parse/3,
			 %% parsing, returning a list of terms
			 parse/4
		       ]).

%% ?- parse(?term, +word list, -tree).
%% ?- parse(?term, +word list, -tree list, -chart size).

:- use_module(grammar).
:- use_module(library(lists)).
:- use_module(library(terms)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parsing an input string, parse/3, parse/4

:- dynamic
	active_edge/6,
	passive_edge/4.

parse(Term, Words, Tree) :-
	read_grammar,
	clear_chart,
	terminal_edges(Words),
	initial_edges(Term, Agenda),
	process(Agenda),
	length(Words, N),
	passive_edge(0, N, Term, Tree),
	\+ terminal(_, Term).

parse(Term, Words, Terms, ChartSize) :-
	findall(Term, parse(Term,Words,_), Terms),
	chart_size(ChartSize).

chart_size(ChartSize) :-
	findall(x, (active_edge(_,_,_,_,_,_);passive_edge(_,_,_,_)), Xs),
	length(Xs, ChartSize).


%% clearing the chart

clear_chart :-
	retractall(active_edge(_,_,_,_,_,_)),
	retractall(passive_edge(_,_,_,_)).
	

%% initializing the input string

terminal_edges(Words) :-
	\+ (
	     nth0(N0, Words, Word),
	     \+ (
		  N is 1+N0,
		  terminal(Word, Term),
		  store([], N0, N, Term, leaf(Word)-[], _)
		)
	   ).


%% initializing the top edges

initial_edges(Term, Edges) :-
	predict(Term, 0, Edges, []).


%% processing the agenda

process([]).
process([edge(ToParse,From,To,Term,Tree)|Agenda0]) :-
	process1(ToParse, From, To, Term, Tree, Agenda, Agenda0),
	process(Agenda).

process1([], From, To, Term, Tree) -->
	resolve_passive(Term, From, To, Tree).
process1([Next|Rest], From, To, Term, Tree) -->
	predict(Next, To),
	resolve_active(Next, Rest, From, To, Term, Tree).

resolve_passive(Next, SoFar, To, PTree) -->
	findall(Edge, (
			active_edge(SoFar, Next, Rest, From, Term, ATree-[PTree|Hole]),
			store(Rest, From, To, Term, ATree-Hole, Edge)
		      ) 
	       ).

resolve_active(Next, Rest, From, SoFar, Term, ATree-[PTree|Hole]) -->
	findall(Edge, (
			passive_edge(SoFar, To, Next, PTree),
			store(Rest, From, To, Term, ATree-Hole, Edge)
		       ) 
	       ).

predict(Term, From) -->
	findall(Edge, (
			production(Term, ToParse),
			store(ToParse, From, From, Term, node(Term,Hole)-Hole, Edge)
		       ) 
	       ).


%% storing edges (passive_edge/3 and active_edge/5)

store([], From, To, Term, Tree-[], Edge) :-
	store_edge(passive_edge(From,To,Term,Tree)),
	Edge = edge([],From,To,Term,Tree).
store([Next|Rest], From, To, Term, Tree, Edge) :-
	( nonvar(Next), Next = {PrologGoal} ->
	    user:PrologGoal,
	    store(Rest, From, To, Term, Tree, Edge)
	;   
	    store_edge(active_edge(To,Next,Rest,From,Term,Tree)),
	    Edge = edge([Next|Rest],From,To,Term,Tree)
	).

store_edge(Edge) :-
	\+ subsumed(Edge),
	assert(Edge).

subsumed(Edge) :-
	functor(Edge, Name, Arity),
	functor(GenEdge, Name, Arity),
	GenEdge,
	subsumes_chk(GenEdge, Edge).




