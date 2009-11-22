

:- op( 1200, xfx, ---> ).
:- op( 1190, xfx, where ).
:- op( 1180, xfx, when ).
:- op( 1150, fx, depth ).

%% also uses bb_get( parser:portray, on )

:- module( parser, [ parse/3,
		     parse/4,
		     generate/3,
		     depth/1,
		     grammar/0
		   ] ).


:- dynamic
	active_edge/6,
	passive_edge/4,
	rule/2.

:- multifile
	user:portray/1,
	user:term_expansion/2.
:- dynamic
	user:portray/1,
	user:term_expansion/2.


:- use_module( library(lists) ).
:- use_module( library(terms) ).
:- use_module( library(random) ).
:- use_module( library(system) ).
:- use_module( library(timeout) ).


%% printing the tree, portray/1

user:portray( Tree ) :-
	bb_get( parser:portray, on ),
	print_tree( Tree, 0 ).


%% coding of terminals and initials

terminal( Word, '$WORD'(Word) ).


%% parsing an input string

parse( Term, Words, Tree ) :-
	read_grammar,
	clear_chart,
	terminal_edges( Words ),
	initial_edges( Term, Agenda ),
	process( Agenda ),
	length( Words, N ),
	passive_edge( 0, N, Term, Tree ),
	\+ terminal( _, Term ).

parse( Term, Words, Terms, ChartSize ) :-
	findall( Term, parse(Term,Words,_), Terms ),
	findall( x, (active_edge(_,_,_,_,_,_);passive_edge(_,_,_,_)), Xs ),
	length( Xs, ChartSize ).


%% clearing the chart

clear_chart :-
	retractall( active_edge(_,_,_,_,_,_) ),
	retractall( passive_edge(_,_,_,_) ).
	

%% initializing the input string

terminal_edges( Words ) :-
	\+ (
	     nth0( N0, Words, Word ),
	     N is 1+N0,
	     terminal( Word, Term ),
	     store( [], N0, N, Term, leaf(Word)-[], _ ),
	     fail
	   ).


%% initializing the top edges

initial_edges( Term, Edges ) :-
	predict( Term, 0, Edges, [] ).


%% processing the agenda

process( [] ).
process( [edge(ToParse,From,To,Term,Tree)|Agenda0] ) :-
	process1( ToParse, From, To, Term, Tree, Agenda, Agenda0 ),
	process( Agenda ).

process1( [], From, To, Term, Tree ) -->
	resolve_passive( Term, From, To, Tree ).
process1( [Next|Rest], From, To, Term, Tree ) -->
	predict( Next, To ),
	resolve_active( Next, Rest, From, To, Term, Tree ).

resolve_passive( Next, SoFar, To, PTree ) -->
	findall( Edge, (
			 active_edge( SoFar, Next, Rest, From, Term, ATree-[PTree|Hole]  ),
			 store( Rest, From, To, Term, ATree-Hole, Edge )
		       ) ).

resolve_active( Next, Rest, From, SoFar, Term, ATree-[PTree|Hole] ) -->
	findall( Edge, (
			 passive_edge( SoFar, To, Next, PTree ),
			 store( Rest, From, To, Term, ATree-Hole, Edge )
		       ) ).

predict( Term, From ) -->
	findall( Edge, (
			 rule( Term, ToParse ),
			 store( ToParse, From, From, Term, node(Term,Hole)-Hole, Edge )
		       ) ).


%% storing edges (passive_edge/3 and active_edge/5)

store( [], From, To, Term, Tree-[], Edge ) :-
	store_edge( passive_edge(From,To,Term,Tree) ),
	Edge = edge([],From,To,Term,Tree).
store( [Next|Rest], From, To, Term, Tree, Edge ) :-
	(   nonvar( Next ),
	    Next = {PrologGoal}
	->  user:PrologGoal,
	    store( Rest, From, To, Term, Tree, Edge )
	;   store_edge( active_edge(To,Next,Rest,From,Term,Tree) ),
	    Edge = edge([Next|Rest],From,To,Term,Tree)
	).

store_edge( Edge ) :-
	\+ subsumed( Edge ),
	assert( Edge ).

subsumed( Edge ) :-
	functor( Edge, Name, Arity ),
	functor( GenEdge, Name, Arity ),
	GenEdge,
	subsumes_chk( GenEdge, Edge ).




%% generating random sentences


generate( Term, Sentence, Tree ) :-
	bb_get( parser:depth, Depth ),
	generate( Depth, Term, Sentence, Tree ).

generate( Depth, Term, Sentence, Tree ) :-
	read_grammar,
	repeat,
	%% time_out( generate(Depth,Term,Tree,Sentence,[]), 1000, success ),
	generate( Depth, Term, Tree, Sentence, [] ),
	!.

generate( s(Depth), LHS, node(LHS,Trees) ) -->
	{ random_rule( LHS, RHS ) },
	generate_rhs( RHS, Depth, Trees ).

generate_rhs( [], _, [] ) --> [].
generate_rhs( [Term|RHS], Depth, OutTrees ) -->
	(   { var(Term) }
	->  generate( Depth, Term, Tree ),
	    { OutTrees = [Tree|Trees] }
	;   { terminal(Word,Term) }
	->  [ Word ],
	    { Tree = leaf(Word),
	      OutTrees = [Tree|Trees] }
	;   { Term = {PrologGoal} }
	->  { random_goal(PrologGoal),
	      OutTrees = Trees }
	;   generate( Depth, Term, Tree ),
	    { OutTrees = [Tree|Trees] }	
	),
	generate_rhs( RHS, Depth, Trees ).
	

random_rule( LHS, RHS ) :-
	random_clause( rule(LHS,RHS), true ).

random_goal( X ) :-
	var( X ),
	!, raise_exception( instantiation_error(X,0) ).
random_goal( (Goal1,Goal2) ) :-
	!, random_goal( Goal1 ), random_goal( Goal2 ).
random_goal( (\+ Goal) ) :-
	!, \+ random_goal( Goal ).
random_goal( (Test->GoalT;GoalF) ) :-
	!, ( random_goal(Test) -> random_goal(GoalT) ; random_goal(GoalF) ).
random_goal( (Test->GoalT) ) :-
	!, ( random_goal(Test) -> random_goal(GoalT) ).
random_goal( (Goal1;Goal2) ) :-
	!, ( random_goal(Goal1) ; random_goal(Goal2) ).
random_goal( ! ) :-
	!, print_message( warning, 'Cuts (!) will not be correctly executed' ).
random_goal( Goal ) :-
	predicate_property( user:Goal, dynamic ),
	!, random_clause( user:Goal, Body ),
	random_goal( Body ).
	%% user:Body.
random_goal( Goal ) :-
	user:Goal.

random_clause( Head, Body ) :-
	bagof( Ref, Head^Body^clause(Head,Body,Ref), Refs ),
	length( Refs, Num ),
	
	random( 0, Num, N ),
	nth0( N, Refs, Ref ),
	%%randseq( Num, Num, Ns ),
	%%member( N, Ns ),
	%%nth( N, Refs, Ref ),
	
	clause( Head, Body, Ref ).


%% the maximun search depth for generation

depth( Depth0 ) :-
	integer( Depth0 ),
	Depth0 > 0,
	conv_depth( Depth0, Depth ),
	bb_put( parser:depth, Depth ).

conv_depth( 0, 0 ).
conv_depth( N0, s(D) ) :-
	N0 > 0, N is N0-1,
	conv_depth( N, D ).


%% errors in the grammar

error_rule(( _ where _ ), where).
error_rule(( _ when _ ), when).
error_rule(( _ ---> _ when (_ where _) ), when).
error_rule(( _ ---> _ when _ where _ ), when).
error_rule(( _ ---> (_ where _) when _ ), when).

user:term_expansion( Rule, _ ) :-
	\+ prolog_load_context( module, parser ),
	error_rule( Rule, Word ),
	raise_exception( context_error(Rule,grammar_rule,[Word]) ).
user:term_expansion( Rule, _ ) :-
	\+ prolog_load_context( module, parser ),
	( Rule = (_--->_ where B when _) ; Rule = (_--->_ where B) ) ->
	\+ only_eqs( B ),
	raise_exception( context_error(Rule,equality_test,[B]) ).

only_eqs( X ) :- var( X ), !, fail.
only_eqs( (G,H) ) :- only_eqs( G ), only_eqs( H ).
only_eqs( _=_ ).


%% reading the grammar and converting to internal format

read_grammar :-
	retractall( rule(_,_) ),
	\+ (
	     user_rule( LHS, RHS0, Where, When ),
	     expand_rule( ({Where},RHS0,{When}), RHS, [] ),
	     assert( rule(LHS,RHS) ),
	     fail
	   ).

expand_rule( X ) -->
	{ var(X) },
	!, [ X ].
expand_rule( (RHS1,RHS2) ) -->
	!, expand_rule( RHS1 ),
	expand_rule( RHS2 ).
expand_rule( [] ) -->
	!, [].
expand_rule( {true} ) -->
	!, [].
expand_rule( [Word|Words] ) -->
	!, { terminal(Word,Term) },
	[ Term ],
	expand_rule( Words ).
expand_rule( Term ) -->
	[ Term ].


user_rule( LHS, RHS, Where, When ) :-
	user:'--->'( LHS, RHS0 ),
	( RHS0 = (RHS where Where when When) ->
	    true
	; RHS0 = (RHS where Where) ->
	    When = true
	; RHS0 = (RHS when When) ->
	    Where = true
	; 
	    RHS = RHS0,
	    Where = true,
	    When = true
	).

%% printing the current grammar

:- op( 1200, yfx, ' --->\n\t' ).
:- op( 1200, yfx, '\n    where\n\t' ).
:- op( 1200, yfx, '\n    when\n\t' ).
:- op( 1000, xfy, ',\n\t' ).

grammar :-
	\+ (
	     user_rule( LHS, RHS, Where, When ),
	     conv_rhs( RHS, RHS1 ),
	     ( Where = true ->
		 ( When = true ->
		     print(( LHS ' --->\n\t' RHS1 ))
		 ;
		     conv_rhs( When, When1 ),
		     print(( LHS ' --->\n\t' RHS1 '\n    when\n\t' When1 ))
		 )
	     ;
		 conv_rhs( Where, Where1 ),
		 ( When = true ->
		     print(( LHS ' --->\n\t' RHS1 '\n    where\n\t' Where1 ))
		 ;
		     conv_rhs( When, When1 ),
		 print(( LHS ' --->\n\t' RHS1 '\n    where\n\t' Where1 '\n    when\n\t' When1 ))
		 )
	     ),
	     write( '.' ), nl,
	     fail
	   ).

conv_rhs( X, XX ) :-
	(   nonvar( X ),
	    X = (Y,Z)
	->  XX = ',\n\t'(Y,ZZ),
	    conv_rhs( Z, ZZ )
	;   XX = X
	).


%% printing a tree

print_tree( leaf(Word), Indent ) :-
	indent( Indent ),
	print( Word ), nl.
print_tree( node(Mother,Daughters), Indent ) :-
	indent( Indent ),
	print( Mother ), nl,
	Indent1 is Indent+1,
	print_trees( Daughters, Indent1 ).

print_trees( [], _ ).
print_trees( [Tree|Trees], Indent ) :-
	print_tree( Tree, Indent ),
	print_trees( Trees, Indent ).

indent( Indent ) :-
	Spaces is Indent*4,
	format( "~t~*|", [Spaces] ).



%% This is to set the random seed for the generation

:- datime( datime(Yr,Mn,Dy,Hr,Mi,Sd) ),
   X is Yr*Mn+1, Y is Dy*Hr+1, Z is Mi*Sd+1,
   setrand( rand(X,Y,Z) ).


%% Setting the standard search depth and pretty-printing

:- depth( 5 ).
:- bb_put( parser:portray, on ).

