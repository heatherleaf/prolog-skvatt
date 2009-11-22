

:- op( 1150, fx, parsetrees ).

:- module( skvatt, [ %% from the parser module
		     parse/3,
		     parse/4,
		     generate/3,
		     depth/1,
		     grammar/0,
		     %% from the FS module
		     feature/3,
		     feature/1,
		     features/1,
		     fs/1,
		     hide/1, show/1, hidden/1,
		     %% from the thistle module
		     start_thistle/0,
		     thistle/1, thistle/2,
		     %% defined in this module
		     parsetrees/1,
		     parse/2,
		     generate/2
		    ] ).

:- use_module( fs ).
:- use_module( parser ).
:- use_module( thistle ).

:- use_module( library(system) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Put the path to the help file here

skvatt_help( '/...PATH.../skvatt/help.txt' ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Do not change this code, please

skvatt_version( 'SKVATT version 0.87, by Peter Ljunglöf, October 2001' ).
file_base( skvatt ).


%% help/0

intro :-
	skvatt_version( V ),
	nl,
	format( "     ~`*t~70|~n", [] ),
	format( "     ~t~w~t~70|~n", [V] ),
	format( "     ~`*t~70|~n", [] ),
	nl.

user:user_help :-
	intro,
	skvatt_help( HelpPath ),
	atom_concat( 'cat ', HelpPath, Cmd ),
	system( Cmd ).


%% parsetrees/1

parsetrees( X ) :-
	(   ( X==on ; X==off )
	->  bb_put( parsetrees, X ) 
	;   raise_exception( domain_error(parsetrees(X),1,on/off,X) )
	).



%% generate/2, generates N different random lists of words
%% outputs all the solutions to the terminal and to sgml files

generate( N, Term ) :-
	integer( N ),
	N > 0,
	reset_file,
	\+ (
	     repeat( N ),
	     ( generate(Term,Words,Tree) -> true ; fail ),
	     print_sentence( Words ), nl,
	     output_result( Term, Tree ), nl,
	     fail
	   ).

repeat( N ) :-
	N > 0,
	(   true
	;   N1 is N-1,
	    repeat( N1 )
	).


%% parse/2, parses a list of words with as a given starting term
%% outputs all the solutions to the terminal and to sgml files

parse( Term, Words ) :-
	reset_file,
	\+ (
	     parse( Term, Words, Tree ),
	     output_result( Term, Tree ), nl,
	     fail
	   ),
	write('No (more) parses.'), nl.


%% outputting the result to terminal and thistle file

output_result( Term, Tree ) :-
	next_file( File ), 
	write( '## Thistle file: ' ), write( File ), nl,
	( bb_get(parsetrees,off) -> Out=Term ; Out=Tree ),
	print( Out ), nl, 
	thistle( File, Out ).


%% reset_file/0 and next_file/1

reset_file :-
	file_base( Base ),
	atom_concat( 'rm -f ', Base, CX ),
	atom_concat( CX, '-*.sgml', Command ),
	system( Command ),
	bb_put( filectr, 0 ).

next_file( File ) :-
	bb_get( filectr, Old ),
	New is Old+1,
	bb_put( filectr, New ),
	file_base( Base ),
	atom_concat( Base, '-', FX ),
	number_chars( New, NewS ),
	atom_chars( NewA, NewS ),
	atom_concat( FX, NewA, FXX ),
	atom_concat( FXX, '.sgml', File ).


%% auxiliary definitions

print_sentence( Sent ) :-
	write( '\"' ), print_list( Sent ), write( '\"' ), nl.

print_list( [] ).
print_list( [W|Ws] ) :-
	print( W ),
	( Ws=[] -> true ; write( ' ' ) ),
	print_list( Ws ).

writenl( A ) :-
	write( A ), nl.



%% print welcome message

:-
	skvatt_version( V ),
	version( V ),
	intro.
