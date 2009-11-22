

:- module(thistle, [ %% starting the thistle interpreter
		     start_thistle/0,
		     %% displaying terms in thistle
		     thistle/1,
		     thistle/2
		    ]).

%% ?- start_thistle.
%% ?- thistle(+term).
%% ?- thistle(+filename, +term).

:- use_module(fs).
:- use_module(library(lists)).
:- use_module(library(system)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% turning off the term expansion while reading this module

:- bb_put(fs:expand, off).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% starting the thistle interpreter, start_thistle/0

start_thistle :-
	absolute_file_name(skvatt('fs.spec'), SpecFile),
	atom_concat('thistle -c ', SpecFile, Command0),
	atom_concat(Command0, ' -startHidden &', Command),
	system(Command).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% thistle interface for FS, thistle/2, thistle/1

thistle(File, Object) :-
	tell(File),
	thistle(Object),
	told.

thistle(Object0) :-
	\+ \+ (
		fs:cfs(Object0, Object, Used),
		fs:instantiate_vars(Object, Used),
		thistle_sgml(Object)
	      ).

%% printing FS in sgml format

thistle_sgml(Object) :-
	write('<!DOCTYPE diagrams SYSTEM "fs.dtd">'), nl,
	thistle_sgml(Object, 0, _).

thistle_sgml(Object) -->
	wrin('<diagrams><diagram>'), 
	wrin('<top_type><a_x_top>'), 
	thistle_object(Object),
	wrout('</a_x_top></top_type>'), 
	wrout('</diagram></diagrams>').

wrin(At, Ind, Ind1) :-
	wr(At, Ind, Ind),
	Ind1 is Ind+1.

wrout(At, Ind1, Ind) :-
	Ind is Ind1-1,
	wr(At, Ind, Ind).

wr(At, Ind, Ind) :-
	indent(Ind), write(At), nl.

pr(At, Ind, Ind) :-
	indent(Ind), print(At), nl.

indent(Ind) :-
	Spaces is Ind*2,
	format("~t~*|", [Spaces]).


%% thistling FS and Refs
thistle_object({X}) -->
	!, wrin('<ref_type><a_x_ref>'),
	wr(X),
	wrout('</a_x_ref></ref_type>').
thistle_object(@({X},Object)) -->
	!, wrin('<refdef_type><a_x_refdef>'),
	wr(X),
	wrout('</a_x_refdef>'),
	wrin('<b_x_refdef>'), 
	thistle_object(Object),
	wrout('</b_x_refdef></refdef_type>').
thistle_object(!(Ref,FVs)) -->
	!, thistle_object(@(Ref,!(FVs))).
thistle_object(!(FVs)) -->
	!, wrin('<record_type><a_x_record>'), 
	thistle_fvs(FVs),
	wrout('</a_x_record></record_type>').
%% thistling trees
thistle_object(node(Mother,Daughters)) -->
	!, wrin('<tree_type><a_x_tree>'),
	thistle_object(Mother),
	wrout('</a_x_tree>'),
	wrin('<b_x_tree>'), 
	thistle_objects(Daughters),
	wrout('</b_x_tree></tree_type>').
thistle_object(leaf(String)) -->
	!, wrin('<string_type><a_x_string>'),
	pr(String),
	wrout('</a_x_string></string_type>').
%% thistling lists
thistle_object(List) -->
	{ is_list(List) },
	!, wrin('<list_type><a_x_list>'), 
	thistle_objects(List),
	wrout('</a_x_list></list_type>').
%% thistling the rest
thistle_object(Term) -->
	wrin('<term_type><a_x_term>'),
	pr(Term),
	wrout('</a_x_term></term_type>').


thistle_fvs([]) --> [].
thistle_fvs([Feat=Val|FVs]) -->
	wrin('<row_type><a_x_row>'), 
	wr(Feat),
	wrout('</a_x_row>'),
	wrin('<b_x_row>'), 
	thistle_object(Val),
	wrout('</b_x_row></row_type>'), 
	thistle_fvs(FVs).


thistle_objects([]) --> [].
thistle_objects([Object|Objects]) -->
	thistle_object(Object),
	thistle_objects(Objects).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% turning on the FS expansion again

:- bb_put(fs:expand, on).

