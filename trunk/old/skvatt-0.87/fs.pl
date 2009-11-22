

:- op( 400, yfx, ? ).
:- op( 200, xfx, @ ).
:- op( 200, xfx, ! ).
:- op( 200, fx, ! ).
:- op( 1150, fx, features ).
:- op( 1150, fx, hide ).
:- op( 1150, fx, show ).

%% can declare terms to alias with
%% user:alias( Term )

%% also uses bb_get( fs:portray, on )
%% and       bb_get( fs:expand, on )


:- module( fs, [ %% getting/setting a certain feature of an FS
		 feature/3,
		 feature/1,
		 %% setting the features
		 features/1,
		 %% creating/checking FS
		 fs/1,
		 %% hiding/showing features
		 hide/1,
		 show/1,
		 hidden/1
	       ] ).

:- use_module( library(lists) ).
:- use_module( library(terms) ).

:- dynamic
	fs/1,
	feature/1,
	feature/3,
	hidden/1.


:- multifile
	user:term_expansion/2,
	user:portray/1.
:- dynamic
	user:term_expansion/2,
	user:portray/1.


%% expanding the FS, term_expansion/2

user:term_expansion( Term0, Term ) :-
	\+ prolog_load_context( module, fs ),
	bb_get( fs:expand, on ),
	bb_put( fs:expand, off ),
	bb_put( fs:expanding, on ),
	catch( (
		 expand_inside( Term0, Term1 ),
		 expand_term( Term1, Term )
	       ),
	       Err,
	       (	
		 bb_put( fs:expanding, off ),
		 bb_put( fs:expand, on ),
		 throw(Err)
	       ) ),
	bb_put( fs:expanding, off ),
	bb_put( fs:expand, on ).


%% printing the FS, portray/1

user:portray( Term ) :-
	bb_get( fs:portray, on ),
	bb_put( fs:portray, off ),
	bb_put( fs:portraying, on ),
	pfs( Term ),
	bb_put( fs:portraying, off ),
	bb_put( fs:portray, on ).




%% declaring the features/1

features( Feats0 ) :-
	features( Feats0, Feats ),
	assert_features( Feats ).

features( (Feat,Feats0), [Feat|Feats] ) :-
	is_feature( Feat ),
	!, features( Feats0, Feats ),
	(   member( Feat, Feats )
	->  error( dupldefinition(Feat,Feats) )
	;   true
	).
features( Feat, [Feat] ) :-
	is_feature( Feat ),
	!.
features( X, _ ) :-
	error( noatomfeature(X) ).

is_feature( Feat ) :-
	atom( Feat ).

is_fs( FS ) :-
	nonvar( FS ),
	fs( FS ).

assert_features( Feats ) :-
	retractall( fs(_) ),
	retractall( feature(_,_,_) ),
	retractall( feature(_) ),
	
	length( [_|Feats], Arity ),
	functor( FS, '$FS', Arity ),
	assert( fs(FS) ),

	\+ (
	     nth( N, Feats, Feat ),
	     arg( N, FS, Val ),
	     \+ (
		  assert( feature(Feat,FS,Val) ),
		  assert( feature(Feat) )
		)
	   ).


%% hiding, showing features

hide( Feat ) :-
	(   \+ is_feature( Feat )
	->  error( nofeature(hide(Feat),Feat) )
	;   hidden( Feat )
	->  true
	;   assert( hidden(Feat) )
	).

show( Feat ) :-
	(   \+ is_feature( Feat )
	->  error( nofeature(hide(Feat),Feat) )
	;   retractall( hidden(Feat) )
	).
	


%% expanding inside a term

expand_inside( Term, Exp ) :-
	expand_inside( Term, Exp, [], Subst ),
	perform_substitution( Subst, Exp ),
	(   cyclic_term( Exp )
	->  warning( cyclic(Exp) )
	;   true
	).

perform_substitution( [], _ ).
perform_substitution( [X=Val|Subst], Term ) :-
	(   X=Val
	->  perform_substitution( Subst, Term )
	;   error( failedsubst(Term,X,Val) )
	).


expand_inside( Term, Term, Subst, Subst ) :-
	var( Term ),
	!.
expand_inside( Term0, Term, Subst0, Subst ) :-
	Term0 =.. [Head|Args0],
	expand_args( Args0, Args, Subst0, Subst1 ),
	(   expand1( Head, Args, Term, Subst1, Subst ) 
	->  true
	;   Term =.. [Head|Args],
	    Subst = Subst1
	).

expand_args( [], [], Subst, Subst ).
expand_args( [Term0|Terms0], [Term|Terms], Subst0, Subst ) :-
	expand_inside( Term0, Term, Subst0, Subst1 ),
	expand_args( Terms0, Terms, Subst1, Subst ).

expand1( @, [X,Term], Term, Subst, [X=Term|Subst] ) :-
	(   var( X )
	->  true 
	;   error( novariable(X@Term,X) )
	).
expand1( ?, [X,Path], SubFS, Subst, [X=FS|Subst] ) :- 
	(   fs( FS ),
	    is_feature( Path ),
	    feature( Path, FS, SubFS ) 
	->  true
	;   error( nofeature(FS?Path,Path) )
	).
expand1( !, [FVs], FS, Subst, Subst ) :-
	(   fs( FS )
	->  expand_fvlist( FVs, FS )
	;   error( nofeatures(!FVs) )
	).
expand1( !, [X,FVs], FS, Subst, [X=FS|Subst] ) :-
	(   var( X )
	->  (   fs( FS )
	    ->  expand_fvlist( FVs, FS )
	    ;   error( nofeatures(X!FVs) )
	    )
	;   error( novariable(X!FVs,X) )
	).

expand_fvlist( X, _ ) :- 
	var( X ), 
	!, write(1), error( instantiation(!X) ).
expand_fvlist( [], _ ).
expand_fvlist( [Assign|FVs], FS ) :-
	(   nonvar( Assign ),
	    Assign = (Feat=Val)
	->  (   is_feature( Feat ),
		feature( Feat, FS, Val1 )
	    ->  (   Val=Val1
		->  expand_fvlist( FVs, FS )
		;   error( duplicated(![Feat=Val|FVs],Val,Val1) )
		)
	    ;   error( nofeature(![Feat=Val|FVs],Feat) )
	    )
	;   error( noassign(![Assign|FVs],Assign) )
	).


%% pretty-printing FS, pfs/12

pfs( Term0 ) :-
	\+ \+ (
		cfs( Term0, Term, Used ),
		instantiate_vars( Term, Used ),
		print( Term )
	      ).

pfs( Term0, Context0 ) :-
	\+ \+ (
		cfs( '$CFS'(Term0,Context0), '$CFS'(Term,_), Used ),
		instantiate_vars( Term, Used ),
		print( Term )
	      ).

cfs( Term0, Term, Used ) :-
	p1( Term0, Term1, [], _, [], Used0 ),
	remove_singles( Used0, [], Used ),
	p2( Term1, Term2, Used ),
	pfs1( Term2, Term, Used ).

instantiate_vars( Term, Used ) :-
	number_vars( Used, 1 ),
	term_variables( Term, Vars ),
	nonumber_vars( Vars ).


%% pass 1

p1( X, X, Seen, Seen, Used, [X|Used] ) :-
	var( X ),
	!.
p1( Term0, Term, Seen0, Seen, Used0, Used ) :-
	\+ alias( Term0 ),
	!, p1t( Term0, Term, Seen0, Seen, Used0, Used ).
p1( Term, Ref, Seen, Seen, Used, [Ref|Used] ) :-
	member( Ref-Term1, Seen ), Term==Term1,
	!.
p1( Term0, '$REF'(Ref,Term), Seen0, Seen, Used0, Used ) :-
	p1t( Term0, Term, [Ref-Term0|Seen0], Seen, [Ref|Used0], Used ).

p1t( Term0, Term, Seen0, Seen, Used0, Used ) :-
	Term0 =.. [Head|Args0],
	p1l( Args0, Args, Seen0, Seen, Used0, Used ),
	Term =.. [Head|Args].

p1l( [], [], Seen, Seen, Used, Used ).
p1l( [Term0|Terms0], [Term|Terms], Seen0, Seen, Used0, Used ) :-
	p1( Term0, Term, Seen0, Seen1, Used0, Used1 ),
	p1l( Terms0, Terms, Seen1, Seen, Used1, Used ).


%% pass 2

p2( X, X, _ ) :-
	var( X ),
	!.
p2( '$REF'(Ref,Term0), Ref@Term, Used ) :-
	eq_member( Ref, Used ),
	!, p2t( Term0, Term, Used ).
p2( '$REF'(_,Term0), Term, Used ) :-
	!, p2t( Term0, Term, Used ).
p2( Term0, Term, Used ) :-
	!, p2t( Term0, Term, Used ).

p2t( Term0, Term, Used ) :-
	Term0 =.. [Head|Args0],
	p2l( Args0, Args, Used ),
	Term =.. [Head|Args].

p2l( [], [], _ ).
p2l( [Term0|Terms0], [Term|Terms], Used ) :-
	p2( Term0, Term, Used ),
	p2l( Terms0, Terms, Used ).


%% pass 3

pfs1( X, X, _ ) :-
	var( X ),
	!.
pfs1( X@FS, X!FVs, Used ) :-
	is_fs( FS ), 
	!, pfvs1( FS, FVs, Used ).
pfs1( FS, !FVs, Used ) :-
	is_fs( FS ), 
	!, pfvs1( FS, FVs, Used ).
pfs1( Term0, Term, Used ) :-
	Term0 =.. [Head|Args0],
	pfs1l( Args0, Args, Used ),
	Term =.. [Head|Args].

pfvs1( FS, FVs, Used ) :-
	bagof( Feat=Val,
	       Val0^(
		     feature(Feat,FS,Val0),
		     \+ hidden(Feat),
		     ( nonvar(Val0) ; eq_member(Val0,Used) ),
		     pfs1(Val0,Val,Used)
		    ),
	       FVs )
	-> true ; FVs = [].

pfs1l( [], [], _ ).
pfs1l( [Term0|Terms0], [Term|Terms], Used ) :-
	pfs1( Term0, Term, Used ),
	pfs1l( Terms0, Terms, Used ).




%% various definitions

alias( Term ) :-
	current_predicate( _, user:alias(_) ),
	user:alias( Term ).
alias( FS ) :-
	is_fs( FS ).
alias( Cyclic ) :-
	cyclic_term( Cyclic ).

eq_member( X, L ) :-
	member( Y, L ), X==Y.

remove_singles( [], Ys, Ys ).
remove_singles( [X|Xs], Ys0, Ys ) :-
       (   eq_member( X, Xs ),
           \+ eq_member( X, Ys0 )
       ->  remove_singles( Xs, [X|Ys0], Ys )
       ;   remove_singles( Xs, Ys0, Ys )
       ).

number_vars( [], _ ).
number_vars( [X|Xs], N0 ) :-
	var( X ),
	!, N is N0+1,
	X = {N0},
	number_vars( Xs, N ).
number_vars( [_|Xs], N ) :-
	number_vars( Xs, N ).

nonumber_vars( [] ).
nonumber_vars( ['_'|Xs] ) :-
	nonumber_vars( Xs ).


%% errors and warnings

error( Error ) :-
	(   bb_get( fs:expanding, on )
	->  bb_put( fs:expanding, off ),
	    bb_put( fs:expand, on )
	;   true
	),
	(   bb_get( fs:portraying, on )
	->  bb_put( fs:portraying, off ),
	    bb_put( fs:portray, on )
	;   true
	),
	print_error( Error ).

print_error( nofeatures(Goal) ) :-
	raise_exception( existence_error(Goal,0,'FEATURES UNDEFINED!',features,_) ).
print_error( duplicated(Goal,X,Y) ) :-
	raise_exception( consistency_error(Goal,X,Y,'the feature is already instantiated') ).
print_error( failedsubst(Term,X,Val) ) :-
	raise_exception( consistency_error(Term,X,Val,'the substitution failed') ).
print_error( instantiation(Goal) ) :-
	raise_exception( instantiation_error(Goal,0) ).
print_error( dupldefinition(Feat,Feats) ) :-
	raise_exception( consistency_error(features(Feat)/Feats,Feat,Feats,'the feature is already defined') ).
print_error( novariable(Goal,Var) ) :-
	raise_exception( type_error(Goal,1,variable,Var) ).
print_error( noassign(Goal,X) ) :-
	raise_exception( type_error(Goal,1,assignmnet,X) ).
print_error( noatomfeature(X) ) :-
	raise_exception( type_error(features(X),1,'atom/feature',X) ).
print_error( nofeature(Goal,Feat) ) :-
	(   bagof( F, feature(F), Feats )
	->  raise_exception( type_error(Goal,1,feature/Feats,Feat) )
	;   print_error( nofeatures(Goal) )
	).

warning( cyclic(_) ) :-
	print_message( warning, 'The term is cyclic!' ).


%% turning on the expansion and printing of FS

:- bb_put( fs:expand, on ).
:- bb_put( fs:portray, on ).


