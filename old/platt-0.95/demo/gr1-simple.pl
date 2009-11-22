

:- use_module( library(platt) ).


S ---> NP, VP, { S:cat = s,
		 NP:cat = np,
		 VP:cat = vp }.

VP ---> V, { VP:cat = vp,
	     V:cat = v }.
VP ---> V, NP, { VP:cat = vp,
		 V:cat = v,
		 NP:cat = np }.

NP --- "pelle", { NP:cat = np }.
NP --- "den", "fina", "flickan", { NP:cat = np }.

V --- "kysser", { V:cat = v }.
V --- "sover", { V:cat = v }.
