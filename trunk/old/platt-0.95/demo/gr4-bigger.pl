

:- use_module( library(platt) ).
:- use_module( library(lists) ).

label(cat). label(subcat). label(struct).

value(Cat) :-
	member( Cat, [s,np,vp,v,pp,p] ).
value(Struct) :-
	member( Struct, [pelle,den,fina,flickan,en,bok,till,kysser,sover,ger] ).


S ---> NP, VP, { S:cat = s,
		 NP:cat = np,
		 VP:cat = vp,
		 S:struct = [NP:struct,VP:struct] }.

VP ---> V, { VP:cat = vp,
	     V:cat = v,
	     V:subcat = [],
	     VP:struct = V:struct }.

VXP ---> V, XP, { VXP:cat = v,
		  V:cat = v,
		  V:subcat = [XP|VXP:subcat],
		  VXP:struct = [V:struct,XP:struct] }.

PP ---> P, NP, { PP:cat = pp,
		 P:cat = p,
		 NP:cat = np,
		 PP:struct = [P:struct,NP:struct] }.

NP --- "pelle", { NP:cat = np, NP:struct = [pelle] }.
NP --- "den", "fina", "flickan", { NP:cat = np, NP:struct = [den,fina,flickan] }.
NP --- "en", "bok", { NP:cat = np, NP:struct = [en,bok] }.

P --- "till", { P:cat = p, P:struct = [till] }.

V --- "kysser", { V:cat = v,
		  V:subcat = [NP],
		  NP:cat = np,
		  V:struct = [kysser] }.
V --- "sover", { V:cat = v,
		 V:subcat = [],
		  V:struct = [sover] }.
V --- "ger", { V:cat = v,
	       V:subcat = [NP,PP],
	       NP:cat = np,
	       PP:cat = pp,
		  V:struct = [ger] }.

