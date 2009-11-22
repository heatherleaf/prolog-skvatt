

:- op(1200, xfx, --->).
:- op(1190, xfx, where).
:- op(1180, xfx, when).


:- module(grammar, [ %% pretty-printing the grammar
		     grammar/0,
		     %% converting the grammar to internal format
		     read_grammar/0,
		     %% internal format for productions and terminals
		     production/2,
		     terminal/2
		   ]).

%% ?- read_grammar.
%% ?- grammar.
%% ?- production(?lhs, ?rhs).
%% ?- terminal(?word, ?term).
%% ?- bb_put(+grammar:portray, +on/off)

:- use_module(library(lists)).
:- use_module(library(terms)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% printing a parse tree, portray/1

:- multifile user:portray/1.
:- dynamic   user:portray/1.

user:portray(Tree) :-
	bb_get(grammar:portray, on),
	print_tree(Tree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% catching errors in the grammar, term_expansion/2

:- multifile user:term_expansion/2.
:- dynamic   user:term_expansion/2.

user:term_expansion(Prod, _) :-
	\+ prolog_load_context(module, grammar),
	nonvar(Prod), Prod = (_ ---> _),
	( check_production(Prod) ->
	    fail
	; 
	    throw(context_error(Prod,production,syntax_error))
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% checking a grammar production, test_production/1
%% splitting the right-hand side, split_RHS/4

check_production((LHS ---> RHS0)) :-
	split_RHS(RHS0, RHS, Where, When), 
	check_LHS(LHS),
	check_RHS(RHS),
	check_where(Where),
	check_when(When).

split_RHS(RHS, RHS, true, true) :-
	\+ wrong_op(RHS), 
	!.
split_RHS((RHS where Where), RHS, Where, true) :-
	\+ wrong_op(RHS), 
	\+ wrong_op(Where),
	!.
split_RHS((RHS when When), RHS, true, When) :-
	\+ wrong_op(RHS), 
	\+ wrong_op(When),
	!.
split_RHS((RHS where Where when When), RHS, Where, When) :-
	\+ wrong_op(RHS), 
	\+ wrong_op(Where), 
	\+ wrong_op(When),
	!.

wrong_op(Term) :- 
	nonvar(Term), 
	member(Term, [(_ :- _), (_ --> _), (_ ---> _), (_ where _), (_ when _), (:- _), (?- _)]).

wrong_op2(Term) :-
	nonvar(Term),
	member(Term, [(_ ; _), (_ -> _), (\+ _), (_ = _), (_ \= _), (_ == _), (_ \== _)]).
	
check_LHS(LHS) :-
	\+ wrong_op(LHS), 
	\+ wrong_op2(LHS), 
	( var(LHS)      ->    true
	; LHS = (_,_)   ->    fail
	; LHS = [_|_]   ->    fail
	; LHS = {_}     ->    fail
	;                     true
	).
	
check_RHS(RHS) :-
	\+ wrong_op(RHS),
	\+ wrong_op2(RHS),
	( var(RHS)      ->    true
	; RHS = (L,R)   ->    check_RHS(L), check_RHS(R)
	; RHS = [_|_]   ->    is_list(RHS)
	;                     true
	).

check_where(Where) :-
	nonvar(Where),
	( Where = (L,R) ->
	    check_where(L),
	    check_where(R)
	;
	    ( Where = true ; Where = (_=_) )
	).

check_when(When) :-
	\+ wrong_op(When).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encoding of terminals,   terminal/2
%% encoding of productions, production/2

terminal(Word, '$WORD'(Word)).

:- dynamic
	production/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% reading the grammar and converting to internal format, read_grammar/0

read_grammar :-
	retractall(production(_,_)),
	\+ (
	     user:'--->'(LHS, RHS0),
	     \+ (
		  split_RHS(RHS0, RHS1, Where, When),
		  expand_production(({Where},RHS1,{When}), RHS, []),
		  assert(production(LHS,RHS))
		)
	   ).

expand_production(X) -->
	{ var(X) },
	!, [ X ].
expand_production((RHS1,RHS2)) -->
	!, expand_production(RHS1),
	expand_production(RHS2).
expand_production([]) -->
	!, [].
expand_production({true}) -->
	!, [].
expand_production([Word|Words]) -->
	!, { terminal(Word, Term) },
	[ Term ],
	expand_production(Words).
expand_production(Term) -->
	[ Term ].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% printing the current grammar, grammar/0

:- op(1200, yfx, ' --->\n\t').
:- op(1200, yfx, '\n    where\n\t').
:- op(1200, yfx, '\n    when\n\t').
:- op(1000, xfy, ',\n\t').

grammar :-
	\+ (
	     user:'--->'(LHS, RHS0),
	     \+ (
		  split_RHS(RHS0, RHS1, Where, When),
		  conv_rhs(RHS1, RHS),
		  print_production(LHS, RHS, Where, When),
		  write('.'), nl
		)
	   ).

conv_rhs(RHS, ConvRHS) :-
	( nonvar(RHS), RHS=(Head,Tail) ->  
	    ConvRHS = (Head ',\n\t' ConvTail),
	    conv_rhs(Tail, ConvTail)
	;   
	    ConvRHS = RHS
	).

print_production(LHS, RHS, true, true) :-
	!, print((LHS ' --->\n\t' RHS)).
print_production(LHS, RHS, true, When0) :-
	!, conv_rhs(When0, When),
	print((LHS ' --->\n\t' RHS '\n    when\n\t' When)).
print_production(LHS, RHS, Where0, true) :-
	!, conv_rhs(Where0, Where),
	print((LHS ' --->\n\t' RHS '\n    where\n\t' Where)).
print_production(LHS, RHS, Where0, When0) :-
	conv_rhs(Where0, Where),
	conv_rhs(When0, When),
	print((LHS ' --->\n\t' RHS '\n    where\n\t' Where '\n    when\n\t' When)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% printing a parse tree, print_tree/1

print_tree(Tree) :-
	print_tree(Tree, 0).

print_tree(leaf(Word), Indent) :-
	indent(Indent),
	print(Word), nl.
print_tree(node(Mother,Daughters), Indent) :-
	indent(Indent),
	print(Mother), nl,
	Indent1 is Indent+1,
	print_trees(Daughters, Indent1).

print_trees([], _).
print_trees([Tree|Trees], Indent) :-
	print_tree(Tree, Indent),
	print_trees(Trees, Indent).

indent(Indent) :-
	Spaces is Indent*4,
	format("~t~*|", [Spaces]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Turning on pretty-printing

:- bb_put(grammar:portray, on).

