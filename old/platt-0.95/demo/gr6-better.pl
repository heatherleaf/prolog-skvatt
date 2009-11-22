

:- use_module( library(platt) ).
:- use_module( library(lists) ).

label(cat). label(subcat). label(struct).

value(Cat) :-
	member( Cat, [s,np,vp,v,pp,p] ).
value(Struct) :-
	member( Struct, [pelle,den,fina,flickan,en,bok,till,kysser,sover,ger] ).

:- setflag( leaflabel, [struct] ).
:- setflag( headlabel, [cat] ).
:- setflag( hidelabel, [cat] ).


S ---> NP, VP, { s(S), np(NP), vp(VP),
		 S:struct = [NP:struct,VP:struct] }.

VP ---> V, { vp(VP), v(V),
	     intrans(V),
	     VP:struct = V:struct }.

VXP ---> V, XP, { v(VXP), v(V),
		  V:subcat = [XP|VXP:subcat],
		  VXP:struct = [V:struct,XP:struct] }.

PP ---> P, NP, { pp(PP), p(P), np(NP),
		 PP:struct = [P:struct,NP:struct] }.

NP --- "pelle", { np(NP) }.
NP --- "den", "fina", "flickan", { np(NP) }.
NP --- "en", "bok", { np(NP) }.

P --- "till", { p(P) }.

V --- "kysser", { v(V), trans(V,NP), np(NP) }.
V --- "sover", { v(V), intrans(V) }.
V --- "ger", { v(V), bitrans(V,NP,PP), np(NP), pp(PP) }.


s(S)  := S:cat = s.
np(S) := S:cat = np.
vp(S) := S:cat = vp.
v(S)  := S:cat = v.
pp(S) := S:cat = pp.
p(S)  := S:cat = p.

intrans(V) := V:subcat = [].
trans(V,XP) := V:subcat = [XP].
bitrans(V,XP,YP) := V:subcat = [XP,YP].


