
:- use_module(library(skvatt)).

:- features cat, struct.



%% icke-terminaler

S ---> NP, VP where 
	S?cat = s,
	NP?cat = np,
	VP?cat = vp,	
	S?struct = {NP?struct,VP?struct}.

XP ---> XP1, Conj, XP2 where
	Conj?cat=conj, 
	XP?cat = XP1?cat,
	XP?cat = XP2?cat,
	XP?struct = {XP1?struct,Conj?struct,XP2?struct}
	when \+ word(XP?cat, _).

NP ---> NP1, PP where
	NP?cat = np, 
	NP1?cat = np, 
	PP?cat = pp,
	NP?struct = {NP1?struct,PP?struct}.

NP ---> PN where
	NP?cat = np, 
	PN?cat = pn,
	NP?struct = {PN?struct}.

NP ---> Det, N where
	NP?cat = np, 
	Det?cat = det, 
	N?cat = n,
	NP?struct = {Det?struct,N?struct}.

PP ---> Prep, NP where
	PP?cat = pp, 
	Prep?cat = prep, 
	NP?cat = np,
	PP?struct = {Prep?struct,NP?struct}.

VP ---> VP1, PP where
	VP?cat = vp, 
	VP1?cat = vp, 
	PP?cat = pp,
	VP?struct = {VP1?struct,PP?struct}.

VP ---> IV where
	VP?cat = vp, 
	IV?cat = iv,
	VP?struct = {IV?struct}.

VP ---> TV, NP where
	VP?cat = vp, 
	TV?cat = tv, 
	NP?cat = np,
	VP?struct = {TV?struct,NP?struct}.


%% terminaler

Term ---> [ Word ] where
	Term?cat = Cat, 
	Term?struct = Word
	when word(Cat, Word).


:- dynamic word/2.

word(pn, pelle).
word(pn, lisa).
word(pn, kiosken).
word(pn, bilen).

word(n, kiosk).
word(n, bil).

word(det, en).

word(prep, i).

word(iv, sover).
word(iv, leker).

word(tv, slår).
word(tv, kramar).

word(conj, och).




