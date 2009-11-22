

:- op(200, fx, !).
:- op(200, xfx, !).
:- op(200, xfx, @).
:- op(400, yfx, ?).
:- op(950, xfx, catcherr).
:- op(1150, fx, features).
:- op(1150, fx, hide).
:- op(1150, fx, show).


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
		hidden/1
	      ]).

%% ?- features +feature, +feature, ..., +feature.
%% ?- fs(?FS).
%% ?- feature(?feature).
%% ?- feature(?feature, ?FS, ?value).
%% ?- hide +feature.
%% ?- show +feature.
%% ?- hidden(?feature).
%% ?- bb_put(+fs:portray, +on/off).
%% ?- bb_put(+fs:expand, +on/off).

%% p[X@V]  ==> X=V, p[X]
%% p[X!FS] ==> p[X@(!FS)]
%% p[FS?F] ==> feature(F,FS,X), p[X]

%% syntax for feature structure:
%% ![f1=v1, f2=v2, ..., fn=vn]

%% can also declare terms to alias with, by asserting
%% user:alias(Term)

:- use_module(library(lists)).
:- use_module(library(terms)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% turning on the expansion and printing of FS

:- bb_put(fs:expand, on).
:- bb_put(fs:portray, on).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% expanding the FS, user:term_expansion/2

:- multifile user:term_expansion/2.
:- dynamic   user:term_expansion/2.

user:term_expansion(Term0, Term) :-
	\+ prolog_load_context(module, fs),
	bb_get(fs:expand, on),
	\+ bb_get(fs:expanding, on),
	bb_put(fs:expanding, on),
	catch( (
		 expand_inside(Term0, Term1), 
		 expand_term(Term1, Term)
	       ),
	       Err,
	       (	
		 bb_put(fs:expanding, off),
		 throw(Err)
	       ) ),
	bb_put(fs:expanding, off).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% printing the FS, user:portray/1

:- multifile user:portray/1.
:- dynamic   user:portray/1.

user:portray(Term) :-
	bb_get(fs:portray, on),
	\+ bb_get(fs:portraying, on),
	bb_put(fs:portraying, on),
	catch( pfs(Term),
	       Err,
	       (
		 bb_put(fs:portraying, off),
		 throw(Err)
	       ) ),
	bb_put(fs:portraying, off).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% declaring the features/1
%% fs/1, feature/1, feature/3

:- dynamic
	fs/1,
	feature/1,
	feature/3.

features(Feats0) :-
	phrase(features(Feats0), Feats), 
	no_doubles(Feats)                         catcherr dupldefinition(Feats),
	assert_features(Feats).

features(X) --> 
	{ var(X),
	  !, error(instantiation(features/1)) }.
features((Feats1,Feats2)) --> 
	!, features(Feats1), features(Feats2).
features({Feats0}) -->
	!, { features(Feats0, Feats, []) }, [Feats].
features(Feat) -->
	{ is_feature(Feat)                        catcherr noatomfeature(Feat) },
	[Feat].

is_feature(Feat) :-
	atom(Feat).

is_fs(FS) :-
	nonvar(FS),
	fs(FS).

assert_features(Feats) :-
	retractall(fs(_)),
	retractall(feature(_,_,_)),
	retractall(feature(_)),

	makeFS([_|Feats], FS),
	assert(fs(FS)),

	\+ (
	     feat_of(1, Feats, FS, Feat, Val),
	     \+ (
		  assert(feature(Feat,FS,Val)),
		  assert(feature(Feat))
		)
	   ).

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
	is_feature(Feat)                          catcherr nofeature(hide(Feat),Feat),
	( hidden(Feat) ->
	    true
	;   
	    assert(hidden(Feat))
	).

show(Feat) :-
	is_feature(Feat)                          catcherr nofeature(hide(Feat),Feat),
	retractall(hidden(Feat)).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% expanding inside a term, expand_inside/2

expand_inside(Term, Exp) :-
	phrase(expand_inside(Term, Exp), Subst),
	perform_substitution(Subst, Exp),
	( cyclic_term(Exp) ->
	    warning(cyclic(Exp))
	;   
	    true
	).

perform_substitution([], _).
perform_substitution([X=Val|Subst], Term) :-
	X=Val                                     catcherr failedsubst(Term,X,Val),
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
	{ var(X)                                  catcherr novariable(X@Term,X) },
	[ X=Term ].
expand1(?, [X,Path], SubFS) -->
	{(   fs(FS)                          )    catcherr nofeatures(X?Path) },
	{(   is_feature(Path),
	     feature(Path, FS, SubFS)        )    catcherr nofeature(X?Path,Path) },
	[ X=FS ].
expand1(!, [FVs], FS) -->
	{ fs(FS)                                  catcherr nofeatures(!FVs),
	  expand_fvlist(FVs, FS) }.
expand1(!, [X,FVs], FS) -->
	{ var(X)                                  catcherr novariable(X!FVs,X),
	  fs(FS)                                  catcherr nofeatures(X!FVs),
	  expand_fvlist(FVs, FS) },
	[ X=FS ].

expand_fvlist(X, _) :- 
	var(X), 
	!, error(instantiation(!X)).
expand_fvlist([], _).
expand_fvlist([Assign|FVs], FS) :-
	(   nonvar(Assign), 
	    Assign=(Feat=Val)                )    catcherr noassign(![Assign|FVs],Assign),
	(   is_feature(Feat), 
	    feature(Feat, FS, Val1)          )    catcherr nofeature(![Feat=Val|FVs],Feat),
	(   Val=Val1                         )    catcherr duplicated(![Feat=Val|FVs],Val,Val1),
	expand_fvlist(FVs, FS).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pretty-printing FS, pfs/1, (pfs/2)

pfs(Term0) :-
	\+ \+ (
		cfs(Term0, Term, Used),
		instantiate_vars(Term, Used),
		print(Term)
	      ).

pfs(Term0, Context0) :-
	\+ \+ (
		cfs('$CFS'(Term0,Context0), '$CFS'(Term,_), Used),
		instantiate_vars(Term, Used),
		print(Term)
	      ).

cfs(Term0, Term, Used) :-
	phrase(p1(Term0, Term1, [], _), Used0),
	remove_singles(Used0, [], Used),
	p2(Term1, Term2, Used),
	pfs1(Term2, Term, Used).

instantiate_vars(Term, Used) :-
	number_vars(Used, 1),
	term_variables(Term, Vars),
	nonumber_vars(Vars).


%% pass 1

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


%% pass 2

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


%% pass 3

pfs1(X, X, _) :-
	var(X),
	!.
pfs1(X@FS, X!FVs, Used) :-
	is_fs(FS), 
	!, pfvs1(FS, FVs, Used).
pfs1(FS, !FVs, Used) :-
	is_fs(FS), 
	!, pfvs1(FS, FVs, Used).
pfs1(Term0, Term, Used) :-
	Term0 =.. [Head|Args0],
	pfs1l(Args0, Args, Used),
	Term =.. [Head|Args].

pfvs1(FS, FVs, Used) :-
	bagof(Feat=Val,
	      Val0^(feature(Feat,FS,Val0),
		    \+ hidden(Feat),
		    (nonvar(Val0) ; eq_member(Val0,Used)),
		    pfs1(Val0,Val,Used)),
	      FVs)  ->  true  ;  FVs = [].

pfs1l([], [], _).
pfs1l([Term0|Terms0], [Term|Terms], Used) :-
	pfs1(Term0, Term, Used),
	pfs1l(Terms0, Terms, Used).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% various definitions

alias(Term) :-
	current_predicate(_, user:alias(_)),
	user:alias(Term).
alias(FS) :-
	is_fs(FS).
alias(Cyclic) :-
	cyclic_term(Cyclic).

eq_member(X, L) :-
	member(Y, L), X==Y.

remove_singles([], Ys, Ys).
remove_singles([X|Xs], Ys0, Ys) :-
       (   eq_member(X, Xs),
           \+ eq_member(X, Ys0)
       ->  remove_singles(Xs, [X|Ys0], Ys)
       ;   remove_singles(Xs, Ys0, Ys)
       ).

number_vars([], _).
number_vars([X|Xs], N0) :-
	var(X),
	!, N is N0+1,
	X = {N0},
	number_vars(Xs, N).
number_vars([_|Xs], N) :-
	number_vars(Xs, N).

nonumber_vars([]).
nonumber_vars(['_'|Xs]) :-
	nonumber_vars(Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% errors and warnings

(Goal catcherr Error) :- Goal -> true ; error(Error).

error(nofeatures(Goal)) :-
	throw(existence_error(Goal,0,'FEATURES UNDEFINED!',features,_)).
error(duplicated(Goal,X,Y)) :-
	throw(consistency_error(Goal,X,Y,'the feature is duplicated')).
error(failedsubst(Term,X,Val)) :-
	throw(consistency_error(Term,X,Val,'the substitution failed')).
error(instantiation(Goal)) :-
	throw(instantiation_error(Goal,0)).
error(dupldefinition(Feats)) :-
	throw(context_error(features(Feats),definition,duplicates)).
error(novariable(Goal,Var)) :-
	throw(type_error(Goal,1,variable,Var)).
error(noassign(Goal,X)) :-
	throw(type_error(Goal,1,assignmnet,X)).
error(noatomfeature(X)) :-
	throw(type_error(features(X),1,'atom/feature',X)).
error(nofeature(Goal,Feat)) :-
	( bagof(F, feature(F), Feats) ->
	    throw(type_error(Goal,1,feature/Feats,Feat))
	;   
	    error(nofeatures(Goal))
	).

warning(cyclic(_)) :-
	print_message(warning, 'The term is cyclic!').



