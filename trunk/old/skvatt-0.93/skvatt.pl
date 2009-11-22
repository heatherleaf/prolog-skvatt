

:- op(1150, fx, parsetrees).


:- module(skvatt, [ %% from the grammar module
		    grammar/0,
		    %% from the parser module
		    parse/3,
		    parse/4,
		    %% from the generator module
		    generate/3,
		    depth/1,
		    %% from the FS module
		    features/1,
		    fs/1,
		    feature/1, feature/3,
		    hide/1, show/1, hidden/1,
		    %% from the thistle module
		    start_thistle/0,
		    thistle/1, thistle/2,
		    %% defined in this module
		    parsetrees/1,
		    parse/2,
		    generate/2
		  ]).

%% ?- grammar.

%% ?- parse(?term, +word list)
%% ?- parse(?term, +word list, -tree).
%% ?- parse(?term, +word list, -tree list, -chart size).

%% ?- generate(+integer, ?term).
%% ?- generate(?term, -word list, -tree).

%% ?- fs(?FS).
%% ?- feature(?feature).
%% ?- feature(?feature, ?FS, ?value).

%% ?- start_thistle.
%% ?- thistle(+term).
%% ?- thistle(+filename, +term).

%% ?- parsetrees +on/off.
%% ?- depth +integer.
%% ?- features +feature, +feature, ..., +feature.
%% ?- hide +feature.
%% ?- show +feature.
%% ?- hidden(?feature).

%% ?- bb_put(+fs:expand, +on/off)
%% ?- bb_put(+fs:portray, +on/off)
%% ?- bb_put(+grammar:portray, +on/off)

:- use_module(fs).
:- use_module(grammar).
:- use_module(chartparser).
:- use_module(generator).
:- use_module(thistle).
:- use_module(library(system)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Version number & file base name, skvatt_version/1, file_base/1

skvatt_version('SKVATT version 0.93, by Peter Ljunglöf, April 2002').
file_base('skvatt-').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Introduction text & help text, intro/0, help/0

intro :-
	skvatt_version(V),
	nl,
	format("     ~`*t~70|~n", []),
	format("     ~t~w~t~70|~n", [V]),
	format("     ~`*t~70|~n", []),
	nl.

user:user_help :-
	absolute_file_name(skvatt('HELP.txt'), File),
	atom_concat('cat ', File, Command),
	system(Command).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hiding and showing the parsetrees/1

parsetrees(X) :-
	(   ( X==on ; X==off )
	->  bb_put(skvatt:parsetrees, X) 
	;   throw(domain_error(parsetrees(X),1,on/off,X) )
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate/2, generates N different random lists of words
%% outputs all the solutions to the terminal and to sgml files

generate(N, Term) :-
	integer(N),
	N > 0,
	reset_file,
	\+ (
	     repeat(N),
	     ( generate(Term, Words, Tree) -> true ; fail ),
	     \+ (
		  print_sentence(Words), nl,
		  output_result(Term, Tree), nl
		)
	   ).

repeat(N) :-
	N > 0,
	( true  ;  N1 is N-1, repeat(N1) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse/2, parses a list of words with as a given starting term
%% outputs all the solutions to the terminal and to sgml files

parse(Term, Words) :-
	reset_file,
	\+ (
	     parse(Term, Words, Tree),
	     \+ (
		  output_result(Term, Tree), nl
		)
	   ),
	write('No (more) parses.'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% outputting the result to terminal and thistle file, output_result/2

output_result(Term, Tree) :-
	next_file(File), 
	write('## Thistle file: '), write(File), nl,
	( bb_get(skvatt:parsetrees, off) -> Out=Term ; Out=Tree ),
	print(Out), nl, 
	thistle(File, Out).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handling thistle files, reset_file/0 and next_file/1

reset_file :-
	file_base(Base),
	atom_concat('rm -f ', Base, CX),
	atom_concat(CX, '-*.sgml', Command),
	system(Command),
	bb_put(skvatt:filectr, 0).

next_file(File) :-
	bb_get(skvatt:filectr, Old),
	New is Old+1,
	bb_put(skvatt:filectr, New),
	file_base(Base),
	number_chars(New, NewS),
	atom_chars(NewA, NewS),
	atom_concat(Base, NewA, BaseNew),
	atom_concat(BaseNew, '.sgml', File).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% printing a sentence, print_sentence/1

print_sentence(Sent) :-
	write('\"'), print_list(Sent), write('\"'), nl.

print_list([]).
print_list([W|Ws]) :-
	print(W),
	( Ws=[] -> true ; write(' ') ),
	print_list(Ws).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% print welcome message

:-
	skvatt_version(V),
	version(V),
	intro.
