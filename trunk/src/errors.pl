
:- module(errors, [error/1,
		   warning/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% throwing errors and warnings

error(Err) :-
	( error(Err, _, _) ->
	    throw(Err)
	; throw(no_skvatt_error(Err)) ).

warning(Wrn) :-
	( warning(Wrn, Str, Args) ->
	    format_warning(Str, Args)
	; throw(no_skvatt_warning(Wrn)) ).


format_error(Str, Args) :-
	write(user_error, '{SKVATT ERROR: '), 
	format(user_error, Str, Args), 
	write(user_error, '}'), nl(user_error).

format_warning(Str, Args) :-
	write(user_error, '{Warning: '), 
	format(user_error, Str, Args), 
	write(user_error, '}'), nl(user_error).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% error terms in Skvatt

%% general errors
error(no_skvatt_error(Err),      "The error term '~w' doesn't exist! PLEASE CONTACT THE AUTHOR!!!",   [Err]).
error(no_skvatt_warning(Wrn),    "The warning term '~w' doesn't exist! PLEASE CONTACT THE AUTHOR!!!",   [Wrn]).
	
%% errors in fs.pl:
error(nofeatures,                "Features are undefined",                              []).
error(duplicated(FS, Feat),      "Feature '~w' is duplicated in FS '~w'",               [Feat, FS]).
error(failedsubst(Term, X, Val), "Substitution '~w = ~w' failed in term `~w'",          [X, Val, Term]).
error(instantiation(Term),       "Instatiation error in term '~w'",                     [Term]).
error(dupldefinition(Feats),     "Duplicated features in the declared features '~w'",   [Feats]).
error(novariable(FS, Var),       "Argument '~w' is not a variable in FS '~w'",          [Var, FS]).
error(noassign(FS, Assign),      "Element '~w' is not an assignment in FS '~w'",        [Assign, FS]).
error(noatomfeature(Feat),       "Feature '~w' is not an atom",                         [Feat]).
error(nofeature(Term, Feat),     "Argument '~w' is not a declared feature in '~w'",     [Feat, Term]).

%% errors in grammar.pl:
error(lhs(LHS, Prod),            "Error in LHS '~w' of production '~w'",                [LHS, Prod]).
error(rhs(RHS, Prod),            "Error in RHS '~w' of production '~w'",                [RHS, Prod]).
error(constraints(Cns, Prod),    "Error in constraints '~w' of production '~w'",        [Cns, Prod]).
error(goal(Goal, Prod),          "Error in goal '~w' of production '~w'",               [Goal, Prod]).
error(production(Prod),          "Error in grammar production '~w'",                    [Prod]).

%% errors in flags.pl
error(noflag(Flag),              "'~w' is not an existing flag",                        [Flag]).
error(noflagvalue(Flag,Value),   "'~w' is not a correct value for flag '~w'",           [Value,Flag]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% warning terms in Skvatt

%% warnings in fs.pl:
warning(cyclic(Term),        "The term '~w' is cyclic",        [Term]).

%% warnings in parser.pl:
warning(nonvar_parseterm,   "The parser term is uninstantiated, which can cause parsing problems",  []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sicstus way of printing errors

:- multifile user:portray_message/2.
:- dynamic   user:portray_message/2.

user:portray_message(error, Err) :-
	error(Err, Str, Args),
	format_error(Str, Args).
