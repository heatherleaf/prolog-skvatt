

:- module(skvatt_help, [introduction/0,
			skvatt_help/0, 
			skvatt_help/1,
			skvatt_help/2,
			skvatt_version/1
		       ]).

:- use_module(utils, [foreach/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Version number, skvatt_version/1

skvatt_version('SKVATT version 1.0b, by Peter Ljunglöf, aug 1999 -- jan 2003').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Introduction text, introduction/0

introduction :-
	skvatt_version(V),
	nl,
	format("~`*t~70|~n    ~w~n~`*t~70|~n", [V]),
	nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% help predicates skvatt_help/012

user:user_help :-
	skvatt_help.

skvatt_help :-
	print_sechelp(skvatt, _),
	skvatt_help(_, _).

skvatt_help(Section) :-
	atom(Section), 
	section(Section),
	!, skvatt_help(Section, _).
skvatt_help(Pred) :-
	skvatt_help(_, Pred).

skvatt_help(Section, Pred) :-
	atom(Pred),
	!, skvatt_help(Section, Pred/_).
skvatt_help(Section, Pred) :-
	foreach(( section(Section),
		  \+ \+ predicate_help(Section, Pred, _, _)
		),
		print_sechelp(Section, Pred)
	       ).

print_sechelp(Section, Pred) :-
	nl,
	section_title(Section, Title),
	format("~`*t~70|~n    ~w~t(~w)    ~70|~n~`*t~70|~n", [Title,Section]), 
	nl,
	section_help(Section, SecHelp),
	print_help(SecHelp), 
	nl,
	foreach( predicate_help(Section, Pred, Goal, PredHelp),
		 print_predhelp(Goal, PredHelp)
	       ).

print_help([]).
print_help([Line|Lines]) :-
	format("    : ~w~n", [Line]),
	print_help(Lines).

print_predhelp(Goal, Help) :-
	format("~w~n", [Goal]),
	print_help(Help).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% help sections

section(skvatt).
section(fs).
section(grammar).
section(parser).
section(thistle).
section(flags).
section(skvatt_help).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% help texts

:- discontiguous section_title/2, section_help/2, predicate_help/4.

%% skvatt.pl

section_title(skvatt, 'Skvatt').
section_help(skvatt, ['This is the Skvatt grammar system containing the modules:',
		      'fs, grammar, parser, generator, thistle, flags and skvatt_help']).

%% fs.pl

section_title(fs, 'Feature structures').
section_help(fs, ['This module defines feture structures (FS)']).

predicate_help(fs, features/0, '?- features +Feature, +Feature, ...',
	       ['set the features to be used in a FS']).
predicate_help(fs, fs/1, '?- fs(?FS).',
	       ['FS is a feature structure']).
predicate_help(fs, feature/1, '?- feature(?Feature).',
	       ['Feature is a declared feature']).
predicate_help(fs, feature/3, '?- feature(?Feature, ?FS, ?Value).',
	       ['Feature has value Value in the feature structure FS']).
predicate_help(fs, '?'/2, '?- ... FS ? Feature ...',
	       ['the functor is expanded to the value of the Feature in the given FS']).
predicate_help(fs, '!'/1, '?- ... ! [Feat=Val,...] ...',
	       ['the functor is expanded to a FS with the features specified']).
predicate_help(fs, '!'/2, '?- ... X ! [Feat=Val,...] ...',
	       ['as the functor ![...] but X is also instantiated to the result',
		'(X is an alias for the FS)']).
predicate_help(fs, '@'/2, '?- ... X @ Term ...',
	       ['as X![...] but Term can be any term',
		'(X is an alias for Term)']).
predicate_help(fs, print/1, '?- print(+Term).',
	       ['pretty-print a term']).
predicate_help(fs, hide/1, '?- hide +Feature.',
	       ['hide a feature when pretty-printing FS']).
predicate_help(fs, show/1, '?- show +Feature.',
	       ['show a feature (default)']).
predicate_help(fs, hidden/1, '?- hidden(?Feature).',
	       ['Feature is a hidden feature']).

%% grammar.pl

section_title(grammar, 'Grammar productions').
section_help(grammar, ['This module defines the grammar format']).

predicate_help(grammar, '--->'/2, 'LHS ---> RHS.',
	       ['definition of a grammar production;',
		'LHS is a grammar term; RHS is a limited goal',
		'(either a grammar term, a terminal (i.e. a list),',
		'a conjunction or a disjunction);',
		'RHS can optionally contain where/2 and when/2;',
		'(RHS where Constraints) and (RHS when Goal)']).

predicate_help(grammar, grammar/0, '?- grammar.',
	       ['pretty-printing the grammar']).

%% parser.pl

section_title(parser, 'Parsing').
section_help(parser, ['This module defines a chartparser working on',
		      'the grammar format in the grammar module']).

predicate_help(parser, parse/2, '?- parse(?Term, +Words).',
	       ['parse a list of Words as the term Term (which can be',
		'uninstantiated). all possible parse trees are written to',
		'the screen and temporary thistle files']).
predicate_help(parser, parse_one/2, '?- parse_one(?Term, +Words).',
	       ['parse a list of Words complying to a Term']).
predicate_help(parser, parse_one/3, '?- parse(?Term, +Words, -Tree).',
	       ['parse a list of Words complying to a Term, giving a parse Tree']).
predicate_help(parser, parse_all/3, '?- parse_all(?Term, +Words, -Terms).',
	       ['parse a String complying to a Term, giving a all possible results']).
predicate_help(parser, final_term/1, '?- final_term(?Term).',
	       ['giving a final parse term, after parsing is done']).
predicate_help(parser, extract_tree/2, '?- extract_tree(?Term, ?Tree).',
	       ['extracting a parse Tree from a Term, after parsing is done']).
predicate_help(parser, print_tree/1, '?- print_tree(+Tree).',
	       ['pretty-printing a parse Tree']).
predicate_help(parser, print_statistics/0, '?- print_statistics.',
	       ['statistics about the latest parse']).
	       
%% generator.pl

section_title(generator, 'Random generation').
section_help(generator, ['This module generates random sentences',
			 'from the current grammar']).

predicate_help(generator, generate/2, '?- generate(+N, ?Term).',
	       ['generate N grammatical phrases for the term Term;',
		'the output is written as strings and',
		'parse trees to the screen and Thistle SGML files']).
predicate_help(generator, generate_words/2, '?- generate_words(?Term, -Words).',
	       ['generate a random grammatical list of Words,',
		'with its parse Tree, that complies with a Term']).

%% thistle.pl

section_title(thistle, 'Thistle, graphically displaying parse trees').
section_help(thistle, ['This module defines an interface to the Thisle graphics package']).

predicate_help(thistle, start_thistle/0, '?- start_thistle.',
	       ['start the Thistle interpreter']).
predicate_help(thistle, thistle/1, '?- thistle(+Term).',
	       ['print a term (e.g. FS or parse tree) in thistle format']).
predicate_help(thistle, thistle/2, '?- thistle(+FileName, +Term).',
	       ['print a term in thistle format to a file']).

%% flags.pl

section_title(flags, 'Handling of flags').
section_help(flags, ['This module defines predicates on flags used by the other modules']).

predicate_help(flags, flags/0, '?- flags.',
	       ['show all the current flags']).
predicate_help(flags, flag_value/2, '?- flag_value(+Flag, ?Value).',
	       ['gets the current value of Flag']).
predicate_help(flags, set_flag/2, '?- set_flag(+Flag, +Value).',
	       ['sets a new value for Flag']).


%% skvatt_help.pl

section_title(skvatt_help, 'Skvatt help texts').
section_help(skvatt_help, ['This module defines help predicates']).

predicate_help(skvatt_help, help/0, '?- help.',
	       ['shows all help texts']).
predicate_help(skvatt_help, skvatt_help/0, '?- skvatt_help.',
	       ['shows all help texts']).
predicate_help(skvatt_help, skvatt_help/1, '?- skvatt_help(+Section/Predicate).',
	       ['gives help for the given section, or the given predicate']).
predicate_help(skvatt_help, skvatt_help/2, '?- skvatt_help(+Section, +Predicate).',
	       ['gives help for the given predicate in the given section']).


