

:- use_module( library(platt) ).

label(cat). label(subcat).

value(s). value(np). value(vp). value(v).
value(pp). value(p).


S ---> NP, VP, { S:cat = s,
		 NP:cat = np,
		 VP:cat = vp }.

VP ---> V, { VP:cat = vp,
	     V:cat = v,
	     V:subcat = [] }.

VXP ---> V, XP, { VXP:cat = v,
		  V:cat = v,
		  V:subcat = [XP|VXP:subcat] }.

PP ---> P, NP, { PP:cat = pp,
		 P:cat = p,
		 NP:cat = np }.

NP --- "pelle", { NP:cat = np }.
NP --- "den", "fina", "flickan", { NP:cat = np }.
NP --- "en", "bok", { NP:cat = np }.

P --- "till", { P:cat = p }.

V --- "kysser", { V:cat = v,
		  V:subcat = [NP],
		  NP:cat = np }.
V --- "sover", { V:cat = v,
		 V:subcat = [] }.
V --- "ger", { V:cat = v,
	       V:subcat = [NP,PP],
	       NP:cat = np,
	       PP:cat = pp }.

