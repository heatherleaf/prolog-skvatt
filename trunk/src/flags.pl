
:- module(flags, [ flags/0, 
		   set_flag/2, 
		   flag_value/2
		 ]).

:- use_module(library(lists), [member/2]).
:- use_module(utils, [foreach/2, or_else/2]).
:- use_module(errors, [error/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% flag predicates

flags :-
	Format = "~p~t~15|~p~t~30|~p~t~60|~p~n",
	format(Format, ['Flag name', 'Current value', 'Accepted values', 'Information']),
	format("~`-t~80|~n", []),
	foreach(( flag_info(Flag, Values, _, Info),
		  flag_value(Flag, Value)
		),
		format(Format, [Flag, Value, Values, Info])
	       ).

set_flag(Flag, Value) :-
	atom(Flag), 
	flag_info(Flag, Values, _, _)  or_else error(noflag(Flag)),
	(   ground(Value),
	    member(Value, Values)   )  or_else error(noflagvalue(Flag,Value)),
	bb_put(Flag, Value).

flag_value(Flag, Value) :-
	bb_get(Flag, Current)
	or_else flag_info(Flag, _, Current, _) 
	or_else error(noflag(Flag)),
	Value = Current.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% flags in Skvatt

flag_info(parsetrees, [on,off],                    off,      'Showing/hiding parse trees').
flag_info(algorithm,  [topdown,bottomup],          bottomup, 'Parsing algorithm').
flag_info(filter,     [topdown,bottomup,both,off], off,      'Parse filter (not implemented yet)').
flag_info(gendepth,   [3,5,7,10,15,20,30,50],      5,        'Max depth for generation').
flag_info(expand_fs,  [on,off],                    on,       'Expansion of feature structures').
flag_info(portray_fs, [on,off],                    on,       'Portraying of features structures').

