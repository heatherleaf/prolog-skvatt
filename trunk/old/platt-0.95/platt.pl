
/*======================================================================
     PLATT (Patr LAboratory Thistle Toolkit) (Patr Langugage And Thistle Trees)
     by Peter Bohlin, August 1999,
     is a PATR interpreter, with pretty-printing components, and
     is developed from Mark Johnson's PRATT via Robin Cooper's modification of
     it to include a left-corner parser (from Pereira and Shieber) called PRATTLE.
     Features:
     * User help
       ( help/0 )
     * Program flags
       ( flags/0, flag/2, setflag/2 )
     * Parsing using a chart parser
       ( parse/2 , parse/3 )
     * Random sentence generation
       ( generate/2, generate/3 )
     * Grammar syntax checking
       ( check/0 )
     * PATR unification of feature structures
       ( prove/1 )
     * Integrated with Jo Calder's Thistle to draw
       nice syntax trees and feature structures
       ( start_thistle/0, thistle/2 )
     * Terminal text output of parse trees and feature structures
       ( pprint/1 )
======================================================================*/

:- op( 1200, xfx, [ ---> , --- , := ] ).


:- module( platt,
	   [
	    %% user help predicate
	    user_help/0,
 
	    %% setting and reading program flags
 	    flags/0,
	    flag/2,
	    setflag/2,

	    %% to parse a string or a list of words
	    parse/2,
	    parse/3,
	    
	    %% to generate a sentence/phrase
	    generate/2,
	    generate/3,

	    %% to check the grammar for syntax errors
	    check/0,

	    %% to prove/unifie a list of goals/PATR paths 
	    prove/1,

	    %% to start the Thistle Interpreter
	    start_thistle/0,

	    %% to output an object to a SGML file, readable by Thistle
	    thistle/2,

	    %% to pretty-print a PLATT term/parse tree to the terminal
	    pprint/1,

	    %% to print prolog strings in readable format (use print/1 to print)
	    portray/1
	   ] ).

:- use_module( library(lists) ).
:- use_module( library(system) ).
:- use_module( library(charsio) ).
:- use_module( library(random) ).


%% This is the file base name, for the SGML files read by Thistle
%% The files are on the form base-N.sgml, where N is 1, 2, 3, 4, ...
%% Also the paths to thistle and the platt.spec file,
%% THESE PATHS MUST BE CORRECT

file_base( platt ).
thistle_path( 'thistle' ).
platt_path( 'platt.spec' ).


%% This is the command line used to start the Thistle interpreter

start_thistle :-
	thistle_path( Thistle ),
	platt_path( Platt ),
	format_to_chars( "~w -c ~w -startHidden &", [Thistle,Platt], Str ),
	atom_chars( Command, Str ),
	system( Command ).


%% This is to set the random seed for the generation

:- datime( datime(Yr,Mn,Dy,Hr,Mi,Sd) ),
   X is Yr*Mn+1, Y is Dy*Hr+1, Z is Mi*Sd+1,
   setrand( rand(X,Y,Z) ).


/*======================================================================
     Thistle flags & user help
     * user_help/0
     * flags/0
     * flag/2
     * setflag/2
======================================================================*/

user_help :-
	writenl( 'PLATT version 0.95, by Peter Ljunglöf' ), nl,
	writenl( 'These are the predicates you can use:' ),
	writenl( ':- help.' ),
	writenl( '     [show this text]' ),
	writenl( ':- flags.' ),
	writenl( '     [show all the flags and their current setting]' ),
	writenl( ':- flag(+Flag,-Value).' ),
	writenl( '     [get the value of a flag]' ),
	writenl( ':- setflag(+Flag,+Value).' ),
	writenl( '     [set the value of a flag]' ),
	writenl( ':- check.' ),
	writenl( '     [check that the grammar is ok]' ),
	writenl( ':- parse(?Cat,+String).' ),
	writenl( '     [parse a String as the category Cat (which can be' ),
	writenl( '      uninstantiated), where the name of the category is the'),
	writenl( '      value in the "catlabel" system flag. all possible parse'),
	writenl( '      trees are written to the screen and Thistle SGML files]' ),
	writenl( ':- parse(+Term,+String,-Tree).' ),
	writenl( '     [parse a String complying to a PLATT Term, giving' ),
	writenl( '      a parse Tree]' ),
	writenl( ':- generate(+N,?Cat).' ),
	writenl( '     [generate N grammatical phrases with the category Cat' ),
	writenl( '      (or any category when Cat is uninstantiated), where' ),
	writenl( '      the name of the category is the value in the "catlabel"'),
	writenl( '      system flag. the output is written as strings and'),
	writenl( '      parse trees to the screen and Thistle SGML files]' ),
	writenl( ':- generate(+Term,-String,-Tree).' ),
	writenl( '     [generate one random grammatical phrase String, with' ),
	writenl( '      its parse Tree, that complies with a PLATT Term]' ),
	writenl( ':- prove(+Goal).' ),
	writenl( '     [prove/unify a goal, e.g. a couple of PLATT paths]' ),
	writenl( ':- pprint(+Object).' ),
	writenl( '     [pretty-print an Object - PLATT term or parse tree]' ),
	writenl( ':- start_thistle.' ),
	writenl( '     [start the Thistle interpreter]' ),
	writenl( ':- thistle(+Object).' ),
	writenl( '     [output an PLATT term or parse tree to a SGML file,' ),
	writenl( '      readable by Thistle]' ).

flags :-
        format( "~n ~a~t~15|~a~t~25+~a~t~15+~a~n~n",
                ['Flag','Possible Values','Current Value','Description'] ),
        \+ ( system_flag(Flag,Values,Info),
             \+ ( system_flag_value(Flag,Current),
                  format( " ~p~t~15|~p~t~25+~p~t~15+~p~n",
                          [Flag,Values,Current,Info] )
             )
        ), 
        format( "~nChange flags with 'setflag(Flag,Value)'~n", [] ).

flag( F, V ):-
        atom( F ),
        system_flag( F, _, _ ),
        !, system_flag_value( F, V ).
flag( Flag, _ ):-
        format( "~n~40c~n Flag doesn't exist: ~w~n~40c~n~n", [0'-,Flag,0'-] ),
        fail.

setflag( Flag, NewValue ):-
        atom( Flag ),
        system_flag( Flag, Values, _ ),
	ok_value( NewValue, Values ),
        !, retractall( system_flag_value(Flag,_OldValue) ),
        assert( system_flag_value(Flag,NewValue) ).
setflag( _, _ ):-
        format( "~n~40c~n Wrong flag or value!~n~40c~n~n", [0'-,0'-] ),
        fail.

ok_value( [], list(_) ) :- !.
ok_value( [V|Vs], list(T) ) :-
	!, ok_value( V, T ), ok_value( Vs, list(T) ).
ok_value( Value, integer ) :-
	!, integer( Value ), Value>1.
ok_value( Value, atom ) :-
	!, atom( Value ).
ok_value( Value, Values ) :-
	member( Value, Values ).

%% the possible flags:

:- dynamic system_flag_value/2.

system_flag( tracer, [on,off,full], 'Tracer for debugging' ).
system_flag( parsetrees, [on,off], 
	     'Printing the whole parsetree or just the top node' ).
system_flag( depth, integer, 'The maximum depth for random generation' ).
system_flag( catlabel, atom, 'The default label for categories' ).
system_flag( leaflabel, list(atom), 'The leaf label (if any) - given the value of the leafs' ).
system_flag( headlabel, list(atom), 'The head label (if any) - printed in front of the FS' ).
system_flag( hidelabel, list(atom), 'Labels that are not shown (if any)' ).

system_flag_value( tracer, off ).
system_flag_value( parsetrees, on ).
system_flag_value( depth, 5 ).
system_flag_value( catlabel, cat ).
system_flag_value( leaflabel, [] ).
system_flag_value( headlabel, [] ).
system_flag_value( hidelabel, [] ).


/*======================================================================
     The generation predicates, by Peter Bohlin
     * generate/2
     * generate/3
======================================================================*/

%% generate/2, generates N different random lists of words

generate( N, Cat ) :-
	N > 0,
	reset_file,
	generate_loop( N, Cat ).

generate_loop( N, Cat ) :-
	N > 0,
	!, flag( catlabel, Label ),
	prove( Start:Label=Cat ),
	generate( Start, Sentence, Tree ),
	sent2string( Sentence, String ),
	nl, print( String ), nl,
	next_file( File ), 
	write( '## Thistle file: ' ), write( File ), nl,
	( flag(parsetrees,off) -> Out=Start ; Out=Tree ),
	pprint( Out ),
	thistle( File, Out ),
	N1 is N-1, 
	generate_loop( N1, Cat ).
generate_loop( _, _ ).

generate( Term, Sentence, Tree ) :- 
	findall( L-R, (user:'--->'(L,R);user:'---'(L,R)), Rules ),
	bb_put( rules, Rules ),
	length( Rules, Num ),
	bb_put( numrules, Num ),
	get_maxdepth( MaxDepth ),
	generate( MaxDepth, Term, Tree, Sentence, [] ).

get_maxdepth( D ) :- flag( depth, MD ), conv_depth( MD, D ).
conv_depth( N, s(D) ) :- N>0, !, conv_depth( N-1, D ).
conv_depth( _, x ).

generate( s(Depth), Phrase, Tree ) -->
	{ bb_get( rules, Rules ),
	  bb_get( numrules, Num ),
	  randseq( Num, Num, Seq ),
	  member( N, Seq ),
	  nth( N, Rules, Phrase-Rest ),
	  goal( Rest, Goal ),
	  prove( Goal ) }, 
	generate( Depth, Phrase, Rest, Tree ).

generate( Depth, Phrase, Rest, node(Phrase,Trees) ) -->
	{ user:'--->'(Phrase,Rest) },
	generate_rest( Depth, Rest, Trees ).
generate( _, Leaf, Rest, node(Leaf,Words) ) -->
	{ user:'---'(Leaf,Rest) },
	generate_leaf( Rest, Words ),
	{ add_leaves_to_term( Leaf, Words ) }.

generate_rest( _, {Goals}, [] ) --> 
	{ prove( Goals ) }.
generate_rest( Depth, (Phrase,Phrases), [Tree|Trees] ) --> 
	generate( Depth, Phrase, Tree ), 
	generate_rest( Depth, Phrases, Trees ).

generate_leaf( {Goals}, [] ) --> 
	{ prove( Goals ) }.
generate_leaf( (Word,Rest), [leaf(Word)|Words] ) -->
	[Word],
	generate_leaf( Rest, Words ).


%% add_leaves_to_term/2

add_leaves_to_term( Term, Leaves ) :-
	flag( leaflabel, Labels ),
	Labels \== [],
	!, findall( Word, ( member(leaf(Str),Leaves), atom_chars(Word,Str) ), Words ),
	member( Label, Labels ),
	prove( Term:Label=Words ).
add_leaves_to_term( _, _ ).

/*======================================================================
     The parsing predicates, a simple chart parser. 
     * parse/2
     * parse/3
======================================================================*/

%% parse/2, parses a string with a given starting category
%% outputs all the solutions to the terminal and to sgml files

parse( Cat, String ) :-
	flag( catlabel, Label ),
	prove( Start:Label=Cat ),
	reset_file,
	parse( Start, String, Tree ),
	next_file( File ), 
	nl, write( '## Thistle file: ' ), write( File ), nl,
	( flag(parsetrees,off) -> Out=Start ; Out=Tree ),
	pprint( Out ),
	thistle( File, Out ),
	fail.
parse( _, _ ) :-
	nl, write('No (more) parses.'), nl.

%% parse/3

parse( Term, String, Tree ) :-
	is_string( String ),
	split_string( String, Sentence ),
	chartparse( Sentence, MaxNode ),
	!, full_edge( 0, MaxNode, TermRes, Tree ),
	prove( Term=TermRes ).


%% the chart parser

:- dynamic partial_edge/6, full_edge/4, node/1.

chartparse( Sent, MaxNode ) :-
	retractall( node(_) ),
	retractall( partial_edge(_,_,_,_,_,_) ),
	retractall( full_edge(_,_,_,_) ),
	nodes( Sent, MaxNode ),
	terminal_edges( Sent ),
	initial_edges,
	increase_chart.

nodes( [], 0 ) :-
	assert( node(0) ).
nodes( [_|L], N1 ) :-
	nodes( L, N ),
	N1 is N+1,
	assert( node(N1) ).

terminal_edges( Sent ) :-
	terminal( Sent, Term, Start, End, Leaves ),
	assert( full_edge(Start,End,Term,node(Term,Leaves)) ),
	fail.
terminal_edges( _ ).

terminal( List, Term, 0, Len, Leaves ) :-
	user:'---'( Term, Rest ),
	terminal( List, Rest, Len, Leaves ),
	add_leaves_to_term( Term, Leaves ).
terminal( [_|List], Term, Start1, End1, Leaves ) :-
	terminal( List, Term, Start, End, Leaves ),
	Start1 is Start+1, End1 is End+1.

terminal( _, {Goal}, 0, [] ) :-
        prove( Goal ).
terminal( [Word|List], (Word,Rest), Len1, [leaf(Word)|Leaves] ) :-
	terminal( List, Rest, Len, Leaves ),
	Len1 is Len+1.

initial_edges :-
	node( Node ),
	user:'--->'( Term, Rest ),
	goal( Rest, Goal ),
	\+ \+ prove( Goal ),
	\+ partial_edge( Node, Node, Rest, Goal, Term, [] ),
	assert( partial_edge(Node,Node,Rest,Goal,Term,[]) ),
	fail.
initial_edges.

increase_chart :-
	partial_edge( Start, End, {_}, G, Term, Trees ),
	prove( G ),
	\+ full_edge( Start, End, Term, node(Term,Trees) ),
	assert( full_edge(Start,End,Term,node(Term,Trees)) ),
	!, increase_chart.
increase_chart :-
	partial_edge( Start, N1, (A,B), G, Term, Trees ),
	full_edge( N1, N2, A, Tree ),
	\+ \+ prove( G ),
	append( Trees, [Tree], NewTrees ),
	\+ partial_edge( Start, N2, B, G, Term, NewTrees ),
	assert( partial_edge(Start,N2,B,G,Term,NewTrees) ),
	!, increase_chart.
increase_chart.


/*======================================================================
     Predicates for proving things with Feature Structures/PATR.
     Originally written by Mark Johnson, modified by Robin Cooper
     and Peter Bohlin
     * prove/1
======================================================================*/

%% a prolog interpreter that understands PATR notation.
%% Just like standard prolog, except that it understands
%% paths of the form Var:a1:a2:a3
%% and represents attribute value lists as the following
%%	@( [ a1:v1 , a2:v2 , a3:v3 | Tail ] )

%% prove/1 is the predicate which starts off the PATR prolog interpreter

prove( X ) :-
	flag( tracer, full ),
	!, formatx( "=> Proving goal: ~p~n", [X] ),
	(   prove( X, 0 )
	->  formatx( "<= Goal proved: ~p~n~n", [X] )
	;   format( "// Goal failed~n~n", [] ), fail
	).
prove( X ) :-
	flag( tracer, on ),
	!, prove( X, 0 ),
	formatx( "// ~p~n", [X] ).
prove( X ) :-
	prove( X, 0 ).

prove( true, _ ) :- !.
prove( (Head,Rest), I ) :- !, 
	prove( Head, I ),
	J is I + 1,
	prove( Rest, J ).

prove( print(T), _ ) :- !, \+ \+ ( numbervars(T), print(T) ).  
prove( nl, _ ) :- !, nl.  
prove( pprint(T), _) :- !, pprint( T ).  

prove( X=Y, Level ) :-
	!, (   flag( tracer, full )
	   ->  formatx( "~d. Considering Unification: ~p~n", [Level,X=Y] )
	   ;   true ),
	unifies(X,Y).

prove( Template, Level ) :-
	current_predicate( _, user:':='(_,_) ),
	user:':='( Template, Goals ),
	!, (   flag( tracer, full )
	   ->  formatx( "~d. Considering Template: ~p~n", [Level,Template] )
	   ;   true ),
	Level1 is Level+1,
	prove( Goals, Level1 ).

prove( Failed, Level ) :-
	formatx( "~d. Failed Goal: ~p~n", [Level,Failed] ),
	fail.


%% unification of two feature structures

unifies( Ax, Bx ) :-
	evaluate( Ax, A ),
	evaluate( Bx, B ),
	unify( A, B ).

%% evaluation of PATR paths

evaluate( Var, Result ) :-
	var( Var ), !, Var=Result.
evaluate( FS:(Atts1:Atts2), Result ) :-
	!, evaluate( FS:Atts1, InterResult ),
	evaluate( InterResult:Atts2, Result ).
evaluate( @(AVs):Att, Result ) :-
	!, set_member( Att:Result, AVs ).
evaluate( Path:Atts, Result ) :- 
	!, evaluate( Path, InterResult ),
	evaluate( InterResult:Atts, Result ).
evaluate( @(AVs), @(AVs) ) :- !.
evaluate( [], _^[] ) :- !.
evaluate( [H|R], V^[H1|R1] ) :-
	!, evaluate( H, H1 ),
	evaluate( R, V^R1 ).
evaluate( X, X ).

set_member( H, [ H | _ ] ) :- !.
set_member( H, [ _ | R ] ) :- set_member( H, R ).

%% unification of (evaluated) PATR paths/feature structures
%% unify defined by rhc

unify( U, U ) :- !.
unify( @(U1), @(U2) ) :- !, unify_lr( U1, U2, T/T ).
unify( [H1|R1], [H2|R2] ) :- !, unify( H1, H2 ), unify( R1, R2 ).
unify( V^[H1|R1], V^[H2|R2] ) :- !, unify( H1, H2 ), unify( V^R1, V^R2 ).

unify_lr( Var, U, In/U ) :- 
	var( Var ), !, Var=In.
unify_lr( [Attr:Val|U1], [Attr:V|U2], In/U2 ) :-
	!, unify( Val, V ),
	unify_lr( U1, In, T/T ).
unify_lr( U1, [Attr:Val|U2], In/T ) :-
	unify_lr( U1, U2, [Attr:Val|In]/T ).


/*======================================================================
     Predicate for checking the grammar
     check/0
======================================================================*/

%% Checking the correctness of the grammar. 

check :-
	check_grammar,
	check_lexicon,
	check_templates.

%% Error predicate

error( _, _ ) :- format( "~t~8|", [] ), fail.
error( lhs, X ) :- format( "LHS not a variable: ~p~n", [X] ).
error( rhs, X ) :- format( "RHS not a variable: ~p~n", [X] ).
error( word, X ) :- format( "RHS not a string: ~p~n", [X] ).
error( no_goals, X ) :- format( "RHS does not end with goals: ~p~n", [X] ).
error( goal, X ) :- format( "Not a correct goal: ~p~n", [X] ).
error( value, X:Goal ) :- format( "Not a correct value (in goal '~p'): ~p~n", [Goal,X] ).
error( path, X:Goal ) :- format( "Not a correct path (in goal '~p'): ~p~n", [Goal,X] ).
error( arg, X ) :- format( "Template argument not a variable: ~p~n", [X] ).
error( cycle, X ) :- format( "A cycle found for template: ~p~n", [X] ).

%% Checking the grammar rules

check_grammar :-
	current_predicate( _, user:'--->'(_,_) ),
	format( "~nChecking grammar rules~n~n", [] ),
	findall( LHS-RHS, user:'--->'(LHS,RHS), Rules ),
	check_grammar( Rules, 1 ).

check_grammar( [], _ ).
check_grammar( [LHS-RHS|Rules], Nr ) :-
	numbervars( LHS-RHS ),
	format( "~t~d. ~5|~p ---> ~p~n", [Nr,LHS,RHS] ),
	check_LHS( LHS ),
	check_RHS( RHS ),
	Nr1 is Nr+1,
	check_grammar( Rules, Nr1 ).

%% Checking the lexicon entries

check_lexicon :-
	current_predicate( _, user:'---'(_,_) ),
	format( "~nChecking lexicon entries~n~n", [] ),
	findall( LHS-RHS, user:'---'(LHS,RHS), Lexicon ),
	check_lexicon( Lexicon, 1 ).

check_lexicon( [], _ ).
check_lexicon( [LHS-RHS|Lexicon], Nr ) :-
	numbervars( LHS-RHS ),
	format( "~t~d. ~5|~p --- ~p~n", [Nr,LHS,RHS] ),
	check_LHS( LHS ),
	check_words( RHS ),
	Nr1 is Nr+1,
	check_lexicon( Lexicon, Nr1 ).

%% Checking the templates

check_templates :-
	current_predicate( _, user:':='(_,_) )
	->	format( "~nChecking template definitions~n~n", [] ),
		findall( Tmpl-Goal, user:':='(Tmpl,Goal), Templates ),
		check_templates( Templates, 1 )
	;	format( "~nThere are no template definitions~n", [] ).

check_templates( [], _ ).
check_templates( [Tmpl-Goal|Templates], Nr ) :-
	numbervars( Tmpl-Goal ),
	format( "~t~d. ~5|~p := ~p~n", [Nr,Tmpl,Goal] ),
	Tmpl =.. [Name|Args],
	check_tmplname( Name ),
	check_tmplargs( Args ),
	check_goals( Goal ),
	check_cycles( Goal, [Tmpl] ),
	Nr1 is Nr+1,
	check_templates( Templates, Nr1 ).

check_tmplname( _ ).

check_tmplargs( [] ).
check_tmplargs( ['$VAR'(_)|Args] ) :- !, check_tmplargs( Args ).
check_tmplargs( [X|Args] ) :- error( arg, X ), check_tmplargs( Args ).

check_cycles( (G,H), Ts ) :- !, check_cycles( G, Ts ), check_cycles( H, Ts ).
check_cycles( T, Ts ) :- 
	functor( T, N, A ), functor( TT, N, A ),
	member( TT, Ts ), !, error( cycle, T ).
check_cycles( T, Ts ) :- user:':='( T, Gs ), !, check_cycles( Gs, [T|Ts] ).
check_cycles( _, _ ).

check_words( (Str,Rest) ) :- is_string( Str ), !, check_words( Rest ).
check_words( (X,Rest) ) :- !, error( word, X ), check_words( Rest ).
check_words( {Goals} ) :- !, check_goals( Goals ).
check_words( X ) :- error( no_goals, X ).	

check_LHS( '$VAR'(_) ) :- !.
check_LHS( X ) :- error( lhs, X ).

check_RHS( ('$VAR'(_),Rest) ) :- !, check_RHS( Rest ).
check_RHS( (X,Rest) ) :- !, error( rhs, X ), check_RHS( Rest ).
check_RHS( {Goals} ) :- !, check_goals( Goals ).
check_RHS( X ) :- error( no_goals, X ).

check_goals( Basic ) :- member( Basic, [true,nl,print(_),pprint(_)] ), !.
check_goals( (G,H) ) :- !, check_goals( G ), check_goals( H ).
check_goals( X=Y ) :- !, check_value( X, X=Y ), check_value( Y, X=Y ).
check_goals( Tmpl ) :- user:':='( Tmpl, _ ), !, Tmpl =.. [_|Args], check_paths( Args, Tmpl ).
check_goals( X ) :- error( goal, X ).

check_value( Path, Goal ) :- ( Path='$VAR'(_) ; Path=_:_ ), !, check_path( Path, Goal ).
check_value( [], _ ) :- !.
check_value( [Val|Vals], Goal ) :- !, check_value( Val, Goal ), check_value( Vals, Goal ).
check_value( Val, _ ) :- current_predicate(_,user:value(_)), user:value( Val ), !.
check_value( X, Goal ) :- error( value, X:Goal ).

check_paths( [], _ ) :- !.
check_paths( [Path|Paths], Goal ) :- !, check_path( Path, Goal ), check_paths( Paths, Goal ).

check_path( '$VAR'(_), _ ) :- !.
check_path( Left:Right, Goal ) :- !, check_path( Left, Goal ), check_labels( Right, Goal ).
check_path( X, Goal ) :- error( path, X:Goal ).

check_labels( Left:Right, Goal ) :- !, check_labels( Left, Goal ), check_labels( Right, Goal ).
check_labels( Lbl, _ ) :- current_predicate(_,user:label(_)), user:label( Lbl ), !.
check_labels( X, Goal ) :- error( path, X:Goal ).


/*======================================================================
     pretty-printers to screen/thistle
     pprint/1
     thistle/2
======================================================================*/

%% outputs a PLATT term/parse tree to the terminal

pprint( Obj1 ) :-
	\+ \+ ( 
		  evaluate( Obj1, Obj2 ),
		  numbervars( Obj2 ), 
		  convobj( Obj2, 1, Obj3 ),
		  %% print( Obj3 ), nl
		  ppt( Obj3, 0 )
	).

%% outputs an object (feature structure/parse tree) to a SGML file

thistle( File, Obj1 ) :-
	\+ \+ ( 
		  evaluate( Obj1, Obj2 ),
		  numbervars( Obj2 ), 
		  convobj( Obj2, 1, Obj3 ),
		  show_thistle( File, Obj3 )
	).

%% outputs an object to a temporary SGML file and starts thistle
%% (not used) 

thistle( Obj1 ) :-
	\+ \+ ( 
		  evaluate( Obj1, Obj2 ),
		  numbervars( Obj2 ), 
		  convobj( Obj2, 1, Obj3 ),
		  show_thistle( Obj3 )
	).
	

%% convobj/3 -- converts objects to referents
%% called by pprint/1, thistle/1,2

convobj( Obj1, N, Obj4 ) :-
	ref( Obj1, N, Ref, Obj2 ),
	ref( Obj2, N, Ref, _ ),
	!, replref( Ref, N, Obj2, Obj3 ),
	N1 is N+1,
	convobj( Obj3, N1, Obj4 ).
convobj( Obj, _, Obj ).

%% ref(Obj1,N,Ref,Obj2), finds a referent Ref in Obj1, renumbers it
%% to N and gives Obj2

ref( @(FS), N, Ref, !(N,@(FS)) ) :- fstail( FS, Ref ).
ref( @(FS), N, Ref, @(FS2) ) :- refs( FS, N, Ref, FS2 ).
ref( !(NN,@(FS)), N, Ref, !(NN,@(FS2)) ) :- refs( FS, N, Ref, FS2 ).
%% next line is commented to prevent lists from being referents
%% ref( Ref^L, N, Ref, !(N,Ref^L) ).
ref( R^L, N, Ref, R^L2 ) :- reflist( L, N, Ref, L2 ).
ref( !(NN,R^L), N, Ref, !(NN,R^L2) ) :- reflist( L, N, Ref, L2 ).
%% next line is commented to prevent leaves from being referents
%% ref( leaf(M,Ls), N, Ref, leaf(M2,Ls) ) :- ref( M, N, Ref, M2 ).
ref( node(M,Ds), N, Ref, node(M2,Ds) ) :- ref( M, N, Ref, M2 ).
ref( node(M,Ds), N, Ref, node(M,Ds2) ) :- reflist( Ds, N, Ref, Ds2 ).
ref( Ref, N, Ref, !(N,Ref) ) :- Ref='$VAR'(_).

refs( [L:V|FS], N, Ref, [L:V2|FS] ) :- ref( V, N, Ref, V2 ).
refs( [L:V|FS], N, Ref, [L:V|FS2] ) :- refs( FS, N, Ref, FS2 ).

reflist( [V|L], N, Ref, [V2|L] ) :- ref( V, N, Ref, V2 ).
reflist( [V|L], N, Ref, [V|L2] ) :- reflist( L, N, Ref, L2 ).

%% replref(Ref,N,Obj1,Obj2), replaces all occurences of Ref in Obj1
%% with number N to give Obj2

replref( Ref, N, @(FS), !(N) ) :- fstail( FS, Ref ), !.
replref( Ref, N, @(FS), @(FS2) ) :- !, replfs( Ref, N, FS, FS2 ).
replref( Ref, N, !(NN,@(FS)), !(NN,@(FS2)) ) :- !, replfs( Ref, N, FS, FS2 ).
replref( Ref, N, Ref^_, !(N) ) :- !.
replref( Ref, N, R^L, R^L2 ) :- !, replist( Ref, N, L, L2 ).
replref( Ref, N, !(NN,R^L), !(NN,R^L2) ) :- !, replist( Ref, N, L, L2 ).
%% next line is commented to prevent leaves from being referents
%% replref( Ref, N, leaf(M,Ls), leaf(M2,Ls) ) :- !, replref( Ref, N, M, M2 ).
replref( Ref, N, node(M,Ds), node(M2,Ds2) ) :- 
	!, replref( Ref, N, M, M2 ), replist( Ref, N, Ds, Ds2 ).
replref( Ref, N, Ref, !(N) ) :- !.
replref( _, _, X, X ).

replfs( Ref, N, [L:V|FS], [L:V2|FS2] ) :- 
	!, replref( Ref, N, V, V2 ), replfs( Ref, N, FS, FS2 ).
replfs( _, _, X, X ).

replist( Ref, N, [V|L], [V2|L2] ) :- 
	!, replref( Ref, N, V, V2 ), replist( Ref, N, L, L2 ).
replist( _, _, [], [] ).

fstail( X, X ) :- X='$VAR'(_).
fstail( [_|X], Y ) :- fstail( X, Y ).


%% ppt/2 -- outputs (converted) objects to the text terminal
%% called by pprint/1

ppt( node(M,Ds), Ind ) :- !, format("~*c",[Ind,0' ]), ppr(M), nl,
	Ind1 is Ind+4, ppts(Ds,Ind1).
%%ppt( leaf(M,Ls), Ind ) :- !, format("~*c",[Ind,0' ]), ppr(M), nl,
%%	Ind1 is Ind+4, ppts(Ls,Ind1).
ppt( leaf(Str), Ind ) :- !, format("~*c",[Ind,0' ]), print( Str ), nl.
ppt( X, Ind ) :- format("~*c",[Ind,0' ]), ppr( X ), nl.

ppts( [T|Ts], Ind ) :- !, ppt(T,Ind), ppts(Ts,Ind).
ppts( [], _ ).

ppr( !(N,'$VAR'(_)) ) :- !, write('<'), print(N), write('>').
ppr( !(N,Obj) ) :- !, write('<'), print(N), write('>'), ppr(Obj).
ppr( !(N) ) :- !, write('<'), print(N), write('>').
%% this is the old way of printing a FS, the new one is below
%% (with head-cats and hidden cats)
%%ppr( @(FS) ) :- !, write('{ '), ppfs(FS), write(' }').
ppr( @(FS) ) :- !,
	(   flag( headlabel, Labels ),
	    member( Label, Labels ),
	    prove( @(FS):Label=Head )
	->  ppr( Head ), write( ': ' )
	;   true ),
	remove_hidden( FS, FFS ),
	write('{ '), ppfs(FFS), write(' }').
ppr( _^List ) :- !, write('[ '), pplist(List), write(' ]').
ppr( '$VAR'(_) ) :- !, write('<>').
ppr( X ) :- print(X).

ppfs( [L:V|FS] ) :-
	!, print(L), write(':'), ppr(V),
	( FS=[_|_] -> write(', ') ; true ),
	ppfs(FS).
ppfs( _ ).

pplist( [V|List] ) :-
	!, ppr(V),
	( List=[_|_] -> write(', ') ; true ),
	pplist(List).
pplist( [] ).


%% remove_hidden/2 -- removes hidden labels from a record.

remove_hidden( FS, FFS ) :-
	flag( hidelabel, Hidden ),
	Hidden \== [],
	!, remove_hidden1( FS, FFS, Hidden ).
remove_hidden( FS, FS ).

remove_hidden1( [C:_|FS], FFS, Hidden ) :-
	member( C, Hidden ),
	!, remove_hidden1( FS, FFS, Hidden ).
remove_hidden1( [CV|FS], [CV|FFS], Hidden ) :-
	!, remove_hidden1( FS, FFS, Hidden ).
remove_hidden1( FS, FS, _ ).


%% show_thistle/2 -- prints thistle-sgml to a file
%% called by thistle/2
%% show_thistle/1 -- prints thistle-sgml to a temporary file and runs Thistle
%% called by thistle/1 (not used)

show_thistle( File, Object ) :-
	tell( File ),
	thistle_sgml( Object ),
	told.

show_thistle( Object ) :-
	tempfile( File ),
	show_thistle( File, Object ),
	commandline( File, Command ),
	system( Command ).

tempfile( File ) :-
	pid( ID ),
	statistics( walltime, [T,_] ),
	format_to_chars( "temp-platt-~w-~w.sgml", [ID,T], Str ),
	atom_chars( File, Str ).

commandline( File, Command ) :-
        thistle_path( Thistle ),
        platt_path( Platt ),
	format_to_chars( "( ~w -c ~w -startHidden ~w ; rm ~w ) &",
			 [Thistle,Platt,File,File], Str ),
	atom_chars( Command, Str ).

%% outputs a (converted) object in Thistle SGML format

thistle_sgml( Object ) :-
	write( '<!DOCTYPE diagrams SYSTEM "platt.dtd">' ), nl,
	write( '<diagrams><diagram>' ), nl,
	write( '<top_type><x_x_top>' ), nl,
	thistle_object( Object ),
	write( '</x_x_top></top_type>' ), nl,
	write( '</diagram></diagrams>' ), nl.

thistle_object( !(Ref,Object) ) :-
	!, write( '<refdef_type><ref_x_refdef>' ),
	write( Ref ),
	write( '</ref_x_refdef><x_x_refdef>' ), nl,
	thistle_object( Object ),
	write( '</x_x_refdef></refdef_type>' ), nl.
thistle_object( !(Ref) ) :-
	!, write( '<ref_type><ref_x_ref>' ),
	write( Ref ),
	write( '</ref_x_ref></ref_type>' ), nl.
%%thistle_object( @(Record) ) :-
%%	!, write( '<record_type><x_x_record>' ), nl,
%%	thistle_record( Record ),
%%	write( '</x_x_record></record_type>' ), nl.
thistle_object( @(Record) ) :-
	flag( headlabel, Labels ),
	member( Label, Labels ),
	prove( @(Record):Label=Head ),
	!, remove_hidden( Record, Record1 ),
	write( '<recordheading_type><x_x_recordheading>' ), nl,
	thistle_object( Head ), 
	write( '</x_x_recordheading><y_x_recordheading>' ), nl,
	write( '<record_type><x_x_record>' ), nl,
	thistle_record( Record1 ),
	write( '</x_x_record></record_type>' ), nl,
	write( '</y_x_recordheading></recordheading_type>' ), nl.
thistle_object( @(Record) ) :-
	!, remove_hidden( Record, Record1 ),
	write( '<record_type><x_x_record>' ), nl,
	thistle_record( Record1 ),
	write( '</x_x_record></record_type>' ), nl.
thistle_object( _^List ) :-
	!, write( '<list_type><x_x_list>' ), nl,
	thistle_objects( List ),
	write( '</x_x_list></list_type>' ), nl.
thistle_object( node(Mother,Daughters) ) :-
	!, write( '<tree_type><mother_x_tree>' ), nl,
	thistle_object( Mother ),
	write( '</mother_x_tree><daughters_x_tree>' ), nl,
	thistle_objects( Daughters ),
	write( '</daughters_x_tree></tree_type>' ), nl.
thistle_object( leaf(String) ) :-
	!, write( '<string_type><x_x_string>' ),
	format( "~s", [String] ),
	write( '</x_x_string></string_type>' ), nl.
thistle_object( '$VAR'(_) ) :-
	!, write( '<term_type><x_x_term>?</x_x_term></term_type>' ), nl.
thistle_object( Term ) :-
	write( '<term_type><x_x_term>' ),
	write( Term ),
	write( '</x_x_term></term_type>' ), nl.

thistle_objects( [] ).
thistle_objects( [Object|Objects] ) :-
	thistle_object( Object ),
	thistle_objects( Objects ).

thistle_record( [Label:Value|Record] ) :-
	!, write( '<row_type><label_x_row>' ),
	write( Label ),
	write( '</label_x_row><x_x_row>' ), nl,
	thistle_object( Value ),
	write( '</x_x_row></row_type>' ), nl,
	thistle_record( Record ).
thistle_record( _ ).


/*======================================================================
     Utility predicates
======================================================================*/

%% Generic output function (prints prolog strings correctly)
%% called via print/1

portray( Str ) :-
	is_string( Str ),
	format( "~c~s~c", [0'",Str,0'"] ).


%% writenl/1

writenl( A ) :- write( A ), nl.


%% numbervars/1

numbervars( X ) :- numbervars( X, 0, _ ).


%% formatx/2

formatx( S, Args ) :-
	\+ \+ ( numbervars( Args ), format( S, Args ) ).


%% goal/1

goal( {G}, G ).
goal( (_,B), G ) :- goal( B, G ).


%% is_string/1
%% Checking that a prolog string really is a prolog string

is_string( [] ).
is_string( [C|S] ) :- integer(C), 32=<C, C=<255, is_string(S).


%% sent2string/2

sent2string( [Wd], Wd ) :- !.
sent2string( [Wd|Wds], Str ) :-
	sent2string( Wds, Str1 ),
	append( Wd, [0' |Str1], Str ).


%% split_string/2

split_string( String, Sentence ) :-
	split_string2( String, Sentence2 ),
	( Sentence2=[""|Sentence] ; Sentence2=Sentence ), !.

split_string2( "", [""] ) :- !.
split_string2( [Space|Str], OutWords ) :-
	space( Space ),
	!, split_string2( Str, Words ),
	( Words=[""|_] -> OutWords=Words ; OutWords=[""|Words] ).
split_string2( [Chr|Str], [[Chr|Word]|Words] ) :-
	split_string2( Str, [Word|Words] ).

space( Space ) :- member( Space, " \t\n\v\f" ).


%% reset_file/0 and next_file/1

reset_file :-
	file_base( Base ),
	format_to_chars( "rm -f ~w-*.sgml", [Base], Str ),
	atom_chars( Command, Str ),
	system( Command ),
	bb_put( filectr, 0 ).

next_file( File ) :-
	bb_get( filectr, Old ),
	New is Old+1,
	bb_put( filectr, New ),
	file_base( Base ),
	format_to_chars( "~w-~w.sgml", [Base,New], Str ),
	atom_chars( File, Str ).


