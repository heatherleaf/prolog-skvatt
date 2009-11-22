

:- module(parser, [ %% parsing
		    parse_one/2,
		    %% parsing to a tree
		    parse_one/3,
		    %% parsing all solutions
		    parse_all/3,
		    %% extracting a tree after parsing
		    extract_tree/2,
		    print_tree/1,
		    %% the chart of passive edges
		    passive_edge/3,
		    final_term/1,
		    %% encoding of the sentence
		    sentence_length/1, 
		    scan_word/3,
		    %% parse statistics
		    parse_statistics/3
		  ]).

:- use_module(library(lists), [nth/3]).
:- use_module(library(terms), [subsumes_chk/2]).
:- use_module(grammar, [terminal/2, deriv_RHS/3, empty_RHS/1]).
:- use_module(utils, [foreach/2, countall/2, or_else/2, time/2]).
:- use_module(flags, [flag_value/2]).
:- use_module(errors, [warning/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dynamic predicates and flags

%% scan_word(?start node, ?end node, ?word)
%% sentence_length(?length)
%% passive_edge(?start node, ?end node, ?LHS)
%% active_edge(?end node, ?start node, ?LHS, ?next term, ?rest of RHS)
%% parse_time(?time)
:- dynamic scan_word/3, sentence_length/1, passive_edge/3, active_edge/5, parse_time/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exported predicates for parsing and post-processing

%% parse_one(?term, +sentence)
parse_one(Term, Input) :-
	nonvar(Term) or_else warning(nonvar_parseterm),
	cleanup,
	initialize(Input),
	time(Time,
	     process(Term)),
	assert(parse_time(Time)),
	final_term(Term).

%% parse_one(?term, +sentence, -parse tree)
parse_one(Term, Input, Tree) :-
	parse_one(Term, Input),
	extract_tree(Term, Tree).

%% parse_all(+term, +sentence, ?terms)
parse_all(Term, Input, Terms) :-
	findall(Term, parse_one(Term,Input), Terms).

%% final_term(?term)
final_term(Term) :-
	sentence_length(N),
	passive_edge(0, N, Term),
	\+ (nonvar(Term), terminal(Term, _)).

%% extract_tree(?term, ?parse tree)
extract_tree(Term, Tree) :-
	sentence_length(N),
	final_term(Term),
	extract_tree(0, N, Term, Tree).

%% parse_statistics(?parse time, ?nr passive edges, ?nr active edges)
parse_statistics(Time, NPassives, NActives) :-
	parse_time(Time),
	countall(passive_edge(_,_,_), NPassives),
	countall(active_edge(_,_,_,_,_), NActives).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% initilalization

:- dynamic filter_topdown/0, filter_bottomup/0, predict_topdown/0, predict_bottomup/0.

%% cleanup
cleanup :- 
	retractall(scan_word(_,_,_)),
	retractall(sentence_length(_)),
	retractall(passive_edge(_,_,_)),
	retractall(active_edge(_,_,_,_,_)),
	retractall(parse_time(_)),
	retractall(predict_topdown),
	retractall(predict_bottomup),
	retractall(filter_topdown),
	retractall(filter_bottomup).

%% initialize(+sentence)
initialize(Input) :-
	assert_algorithm,
	foreach(( nth(To, Input, Word),
		  From is To - 1
		),
		assert(scan_word(From, To, Word))),
	length(Input, Len),
	assert(sentence_length(Len)).

%% assert_algorithm
assert_algorithm :-
	( flag_value(algorithm, topdown)  -> assert(predict_topdown)
	; flag_value(algorithm, bottomup) -> assert(predict_bottomup)
	),
	( flag_value(filter, topdown)  -> assert(filter_topdown)
	; flag_value(filter, bottomup) -> assert(filter_bottomup)
	; flag_value(filter, both)     -> assert(filter_topdown), assert(filter_bottomup)
	; true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the parsing process

%% process(+initial term)
process(Start) :-
	%% initial top-down prediction
	( (predict_topdown ; filter_topdown) -> 
	    add_edge(active_edge(0, 0, '<$start$>', Start, '<$end$>'))
	; true ),
	%% initial kilbury bottom-up predict/scan
	foreach(( scan_word(From, To, Word),
		  terminal(Term, Word)
		),
		add_edge(passive_edge(From, To, Term))
	       ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% adding one edge to the chart

%% add_edge(+start node, +end node, +LHS, +RHS)
add_edge(From, To, LHS, RHS) :-
	( empty_RHS(RHS),
	    add_edge(passive_edge(From, To, LHS))
	; deriv_RHS(RHS, Next, Rest),
	    add_edge(active_edge(To, From, LHS, Next, Rest))
	).

%% add_edge(+edge)
add_edge(Edge) :-
	\+ subsuming_edge(Edge),
	assert(Edge),
	new_edges(Edge).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% inferring new edges from a newly added edge

%% new_edges(+edge)
new_edges(active_edge(To, From, LHS, Next, Rest)) :-
	%% complete
	foreach(( passive_edge(To, To1, Next)
		),
		add_edge(From, To1, LHS, Rest)),
	%% top-down predict
	foreach(( predict_topdown,
		  user:(Next ---> RHS),
		  filter_bottomup(RHS, To)
		),
		add_edge(To, To, Next, RHS)),
	%% bottom-up, empty productions
	foreach(( predict_bottomup,
		  user:(Next ---> RHS),
		  empty_RHS(RHS)
		),
		add_edge(passive_edge(To, To, Next))).
new_edges(passive_edge(From, To, Found)) :-
	%% complete
	foreach(( active_edge(From, From0, LHS, Found, RHS)
		),
		add_edge(From0, To, LHS, RHS)),
 	%% kilbury bottom-up predict 
	foreach(( predict_bottomup,
		  user:(LHS ---> RHS),
		  deriv_RHS(RHS, Found, Rest),
		  filter_topdown(LHS, From),
		  filter_bottomup(Rest, To)
		),
		add_edge(From, To, LHS, Rest)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% predicates for edge existence/subsumption

%% subsuming_edge(+edge)
subsuming_edge(Edge) :-
	similar_edge(Edge, Subsumer),
	Subsumer,
	subsumes_chk(Subsumer, Edge).

%% similar_edge(+edge, ?edge)
similar_edge(passive_edge(From, To, _), passive_edge(From, To, _)).
similar_edge(active_edge(To, From, _, _, _), active_edge(To, From, _, _, _)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% filtering the edges

%% filter_topdown(+term, +node)
filter_topdown(LHS, Node) :- 
	once(( \+ filter_topdown
	     ; 
	       active_edge(Node, _, _, TopLHS, _),
	       catlink(TopLHS, LHS)
	     )).

%% filter_bottomup(+RHS, +node)
filter_bottomup(RHS, Node) :- 
	once(( \+ filter_bottomup
	     ;
	       empty_RHS(RHS)
	     ;
	       deriv_RHS(RHS, Next, _),
	       scan_word(Node, _, Word),
	       wordlink(Next, Word)
	     )).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extracting parse trees

%% extract_tree(+start node, +end node, +term, ?parse tree)
extract_tree(From, To, Term, Word) :-
	terminal(Term, Word),
	scan_word(From, To, Word).
extract_tree(From, To, Term, Term^Trees) :-
	user:(Term ---> RHS),
	path(RHS, From, To, Path),
	extract_trees(Path, Trees).

%% extract_trees(+passive edges, ?parse trees)
extract_trees([], []).
extract_trees([From-To:Term|Path], [Tree|Trees]) :-
	extract_tree(From, To, Term, Tree),
	extract_trees(Path, Trees).

%% path(+RHS, +start node, +end node, ?passive edges)
path(RHS, From, From, []) :-
	empty_RHS(RHS).
path(RHS, From, To, [From-Mid:Next|Path]) :-
	deriv_RHS(RHS, Next, Rest),
	passive_edge(From, Mid, Next),
	Mid =< To,
	path(Rest, Mid, To, Path).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% printing a parse tree, print_tree/1

print_tree(Tree) :-
	print_tree(Tree, '\t').

print_tree(Tree, Indent) :-
	write(Indent),
	( nonvar(Tree), Tree = Mother^Daughters ->
	    print(Mother), nl,
	    atom_concat(Indent, '    ', Indent1),
	    print_trees(Daughters, Indent1)
	; print(Tree), nl ).

print_trees([], _).
print_trees([Tree|Trees], Indent) :-
	print_tree(Tree, Indent),
	print_trees(Trees, Indent).

