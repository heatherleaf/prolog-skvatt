

:- module(fs, [ %% setting the features
		features/1,
		%% creating/checking FS
		fs/1,
		%% checking/listing feature
		feature/1,
		%% getting/setting a certain feature of an FS
		feature/3,
		%% hiding/showing features
		hide/1,
		show/1,
		hidden/1,
		%% printing/converting FS
		print_FS/1, 
		convert_FS/2
	      ]).

:- op(200, fx, !).
:- op(200, xfx, !).
:- op(200, xfx, @).
:- op(400, yfx, ?).
:- op(1150, fx, features).
:- op(1150, fx, hide).
:- op(1150, fx, show).

%% p[X@V]  ==> X=V, p[X]
%% p[X!FS] ==> p[X@(!FS)]
%% p[FS?F] ==> feature(F,FS,X), p[X]

%% syntax for feature structures:
%% ![f1=v1, f2=v2, ..., fn=vn]

:- use_module(library(lists), [member/2, no_doubles/1]).
:- use_module(library(terms), [cyclic_term/1]).
:- use_module(utils, [foreach/2, or_else/2, instantiate_variables/2, eq_member/2, remove_singles/2]).
:- use_module(flags, [flag_value/2]).
:- use_module(errors, [error/1, warning/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% expanding the FS, user:term_expansion/2

:- multifile user:term_expansion/2.
:- dynamic   user:term_expansion/2.

:- dynamic   is_expanding/0.

user:term_expansion(Term0, Term) :-
	\+ (   prolog_load_context(module, Module),
	       member(Module, [fs,flags,utils,errors])
	   ),
	flag_value(expand_fs, on),
	\+ is_expanding,
	assert(is_expanding),
	catch((   expand_inside(Term0, Term1), 
		  expand_term(Term1, Term)
	      ),
	      Err,
	      (   retractall(is_expanding),
		  throw(Err)
	      )),
	retractall(is_expanding).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% printing the FS, user:portray/1

:- multifile user:portray/1.
:- dynamic   user:portray/1.

:- dynamic   is_printing/0.

user:portray(Term) :-
	flag_value(portray_fs, on),
	\+ is_printing,
	assert(is_printing),
	catch(print_FS(Term), 
	      Err,
	      (   retractall(is_printing),
		  throw(Err)
	      )),
	retractall(is_printing).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% declaring the features/1
%% fs/1, feature/1, feature/3

:- dynamic
	fs/1,
	feature/1,
	feature/3.

features(Feats0) :-
	phrase(features(Feats0), Feats), 
	no_doubles(Feats)                         or_else error(dupldefinition(Feats)),
	assert_features(Feats).

features(X) --> 
	{ var(X),
	  !, error(instantiation(features/1)) }.
features((Feats1,Feats2)) --> 
	!, features(Feats1), features(Feats2).
features({Feats0}) -->
	!, { features(Feats0, Feats, []) }, [Feats].
features(Feat) -->
	{ is_feature(Feat)                        or_else error(noatomfeature(Feat)) },
	[Feat].

is_feature(Feat) :-
	atom(Feat).

is_FS(FS) :-
	nonvar(FS),
	fs(FS).

assert_features(Feats) :-
	retractall(fs(_)),
	retractall(feature(_,_,_)),
	retractall(feature(_)),

	makeFS([_|Feats], FS),
	assert(fs(FS)),

	foreach(feat_of(1, Feats, FS, Feat, Val),
		(   assert(feature(Feat,FS,Val)),
		    assert(feature(Feat))
		)).

makeFS(Feats, FS) :-
	length(Feats, Arity),
	functor(FS, '$FS', Arity).

feat_of(N, [Feat0|Feats], FS, Feat, Val) :-
	(
	  arg(N, FS, Val0),
	  ( is_feature(Feat0) -> Feat = Feat0, Val = Val0
	  ; is_list(Feat0)    -> makeFS(Feat0, Val0), feat_of(1, Feat0, Val0, Feat, Val) )
	;
	  N1 is N+1,
	  feat_of(N1, Feats, FS, Feat, Val)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hiding and showing features, hide/1, show/1 and hidden/1

:- dynamic
	hidden/1.

hide(Feat) :-
	is_feature(Feat)                          or_else error(nofeature(hide(Feat),Feat)),
	( hidden(Feat) ->
	    true
	;   
	    assert(hidden(Feat))
	).

show(Feat) :-
	is_feature(Feat)                          or_else error(nofeature(hide(Feat),Feat)),
	retractall(hidden(Feat)).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% expanding inside a term, expand_inside/2

expand_inside(Term, Exp) :-
	phrase(expand_inside(Term, Exp), Subst),
	perform_substitution(Subst, Exp),
	( cyclic_term(Exp) ->
	    warning(cyclic(Term))
	;   
	    true
	).

perform_substitution([], _).
perform_substitution([X=Val|Subst], Term) :-
	X=Val                                     or_else error(failedsubst(Term,X,Val)),
	perform_substitution(Subst, Term).


expand_inside(Term, Term) -->
	{ var(Term) },
	!.
expand_inside(Term0, Term) -->
	{ Term0 =.. [Head|Args0] },
	expand_args(Args0, Args),
	( expand1(Head, Args, Term) ->
	    []
	;   
	    { Term =.. [Head|Args] }
	).

expand_args([], []) --> [].
expand_args([Term0|Terms0], [Term|Terms]) -->
	expand_inside(Term0, Term),
	expand_args(Terms0, Terms).

expand1(@, [X,Term], Term) -->
	{ var(X)                                  or_else error(novariable(X@Term,X)) },
	[ X=Term ].
expand1(?, [X,Path], SubFS) -->
	{(   fs(FS)                          )    or_else error(nofeatures) },
	{(   is_feature(Path),
	     feature(Path, FS, SubFS)        )    or_else error(nofeature(X?Path,Path)) },
	[ X=FS ].
expand1(!, [FVs], FS) -->
	{ fs(FS)                                  or_else error(nofeatures),
	  expand_fvlist(FVs, FS) }.
expand1(!, [X,FVs], FS) -->
	{ var(X)                                  or_else error(novariable(X!FVs,X)),
	  fs(FS)                                  or_else error(nofeatures),
	  expand_fvlist(FVs, FS) },
	[ X=FS ].

expand_fvlist(X, _) :- 
	var(X), 
	!, error(instantiation(!X)).
expand_fvlist([], _).
expand_fvlist([Assign|FVs], FS) :-
	(   nonvar(Assign), 
	    Assign=(Feat=Val)                )    or_else error(noassign(FS,Assign)),
	(   is_feature(Feat), 
	    feature(Feat, FS, Val1)          )    or_else error(nofeature(FS,Feat)),
	(   Val=Val1                         )    or_else error(duplicated(FS,Feat)),
	expand_fvlist(FVs, FS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pretty-printing FS, print_FS/1, print_FS/2

print_FS(Term0) :-
	\+ \+ (
		convert_FS(Term0, Term),
		print(Term)
	      ).

convert_FS(Term0, Term) :-
	phrase(p1(Term0, Term1, [], _), Used0),
	remove_singles(Used0, Used),
	p2(Term1, Term2, Used),
	p3(Term2, Term, Used),
	instantiate_variables(Term, Used).

%% pass 1: collecting variables and converting to an aliased term

p1(X, X, Seen, Seen) -->
	{ var(X) },
	!, [ X ].
p1(Term0, Term, Seen0, Seen) -->
	{ \+ alias(Term0) },
	!, p1t(Term0, Term, Seen0, Seen).
p1(Term, Ref, Seen, Seen) -->
	{ member(Ref-Term1, Seen), Term==Term1 },
	!, [ Ref ].
p1(Term0, '$REF'(Ref,Term), Seen0, Seen) -->
	[ Ref ], 
	p1t(Term0, Term, [Ref-Term0|Seen0], Seen).

p1t(Term0, Term, Seen0, Seen) -->
	{ Term0 =.. [Head|Args0] },
	p1l(Args0, Args, Seen0, Seen),
	{ Term =.. [Head|Args] }.

p1l([], [], Seen, Seen) --> [].
p1l([Term0|Terms0], [Term|Terms], Seen0, Seen) -->
	p1(Term0, Term, Seen0, Seen1),
	p1l(Terms0, Terms, Seen1, Seen).

alias(FS) :-
	is_FS(FS).
alias(Cyclic) :-
	cyclic_term(Cyclic).


%% pass 2: unaliasing subterms that only occurs once

p2(X, X, _) :-
	var(X),
	!.
p2('$REF'(Ref,Term0), Ref@Term, Used) :-
	eq_member(Ref, Used),
	!, p2t(Term0, Term, Used).
p2('$REF'(_,Term0), Term, Used) :-
	!, p2t(Term0, Term, Used).
p2(Term0, Term, Used) :-
	!, p2t(Term0, Term, Used).

p2t(Term0, Term, Used) :-
	Term0 =.. [Head|Args0],
	p2l(Args0, Args, Used),
	Term =.. [Head|Args].

p2l([], [], _).
p2l([Term0|Terms0], [Term|Terms], Used) :-
	p2(Term0, Term, Used),
	p2l(Terms0, Terms, Used).


%% pass 3: converting FS to output format

p3(X, X, _) :-
	var(X),
	!.
p3(X@FS, X!FVs, Used) :-
	is_FS(FS), 
	!, p3fs(FS, FVs, Used).
p3(FS, !FVs, Used) :-
	is_FS(FS), 
	!, p3fs(FS, FVs, Used).
p3(Term0, Term, Used) :-
	Term0 =.. [Head|Args0],
	p3l(Args0, Args, Used),
	Term =.. [Head|Args].

p3l([], [], _).
p3l([Term0|Terms0], [Term|Terms], Used) :-
	p3(Term0, Term, Used),
	p3l(Terms0, Terms, Used).

p3fs(FS, FVs, Used) :-
	findall(Feat, feature(Feat), Feats),
	filter_feats(Feats, FVs, FS, Used).


filter_feats([], [], _, _).
filter_feats([Feat | Feats], [Feat=Val | FVs], FS, Used) :-
	\+ hidden(Feat),
	feature(Feat, FS, Val0),
	( nonvar(Val0) ; eq_member(Val0, Used) ),
	!, p3(Val0, Val, Used),
	filter_feats(Feats, FVs, FS, Used).
filter_feats([_ | Feats], FVs, FS, Used) :-
	filter_feats(Feats, FVs, FS, Used).




