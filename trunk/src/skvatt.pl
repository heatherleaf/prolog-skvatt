

:- module(skvatt, [ %% defined in this module
		    parse/2,
		    generate/2,
		    print_statistics/0,
		    %% module grammar
		    grammar/0,
		    %% module parser
		    parse_one/2, parse_one/3, parse_all/3,
		    final_term/1, extract_tree/2, print_tree/1,
		    %% module generator
		    generate_corpus/2,
		    generate_words/2,
		    %% module fs
		    features/1, 
		    fs/1, feature/1, feature/3,
		    hide/1, show/1, hidden/1,
		    %% module thistle
		    start_thistle/0,
		    thistle/1, thistle/2,
		    %% module flags
		    flags/0, set_flag/2, flag_value/2,
		    %% module skvatt_help
		    skvatt_help/0, skvatt_help/1, skvatt_help/2
		  ]).

:- use_module([ grammar, 
		parser, 
		generator,
		fs,
		thistle,
		flags,
		skvatt_help
	      ]).

:- use_module(utils, [foreach/2, fromto/3]).
:- use_module(library(system), [system/1]).
:- use_module(library(lists), [nth/3]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate/2, generates a number of random sentences
%% outputs the solutions to the terminal and to sgml files

generate(N, Term) :-
	integer(N),
	N > 0,
	nl,
	delete_skvatt_files,
	foreach( fromto(1, N, K),
		 (   generate_words(Term, Words),
		     print_sentence(Words),
		     ( flag_value(parsetrees, on) ->
			 parse_one(Term, Words),
			 output_trees(K, Term)
		     ;
			 output_term(K, 0, Term)
		     )
		 ) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse/2, parses a list of words with as a given starting term
%% outputs all the solutions to the terminal and to sgml files

parse(Term, Words) :-
	parse_all(Term, Words, Terms),
	nl,
	print_sentence(Words),
	delete_skvatt_files,
	foreach( nth(N, Terms, Term),
		 ( flag_value(parsetrees, on) ->
		     output_trees(N, Term)
		 ;
		     output_term(N, 0, Term)
		 )),
	write('No (more) parses.'), nl,
	print_statistics.

print_statistics :-
	parse_statistics(Time, PEdges, AEdges),
	nl,
	format("Total elapsed time: ~3d s~n", [Time]),
	format("Chart size: ~d active, ~d passive edges~n", [AEdges, PEdges]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% outputting the result to terminal and thistle file,
%% output_term/3, output_trees/2

output_term(N, M, Term) :-
	create_skvatt_file(N, M, File),
	write('## Thistle file: '), write(File), nl,
	nl,
	\+ \+ (
		convert_FS(Term, PTerm),
		print_tree(PTerm), nl
	      ),
	thistle(File, Term).

output_trees(N, Term) :-
	findall(Tree, extract_tree(Term, Tree), Trees),
	foreach( nth(M, Trees, Tree),
		 output_term(N, M, Tree)
	       ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handling thistle files, file_base/1,
%% delete_skvatt_files/0, create_skvatt_file/3

file_base('skvatt-').

delete_skvatt_files :-
	file_base(Base),
	atom_concat('rm -f ', Base, CX),
	atom_concat(CX, '*.sgml', Command),
	system(Command).

create_skvatt_file(N, M, File) :-
	file_base(Base),
	number_codes(N, NStr),
	number_codes(M, MStr),
	atom_codes(NAtom, NStr),
	atom_codes(MAtom, [0'-|MStr]),
	atom_concat(Base, NAtom, NBase),
	atom_concat(NBase, MAtom, NMBase),
	atom_concat(NMBase, '.sgml', File).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% printing a sentence, print_sentence/1

print_sentence(Sent) :-
	write('\"'), print_list(Sent), write('\"'), nl, nl.

print_list([]).
print_list([W|Ws]) :-
	print(W),
	( Ws=[] -> true ; write(' ') ),
	print_list(Ws).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% print welcome message

:- introduction.

