
:- use_module(library(skvatt)).

:- features cat, struct.



%% icke-terminaler

S![cat=s] ---> 
	NP![cat=np], 
	VP![cat=vp]
	where S?struct = {NP?struct,VP?struct}.

XP![cat=X] ---> 
	XP1![cat=X], 
	Conj![cat=conj], 
	XP2![cat=X]
	where XP?struct = {XP1?struct,Conj?struct,XP2?struct}
	when \+ word(X, _).

NP![cat=np] ---> 
	NP1![cat=np], 
	PP![cat=pp]
	where NP?struct = {NP1?struct,PP?struct}.
NP![cat=np] ---> 
	PN![cat=pn]
	where NP?struct = {PN?struct}.
NP![cat=np] ---> 
	Det![cat=det], 
	N![cat=n]
	where NP?struct = {Det?struct,N?struct}.

PP![cat=pp] ---> 
	Prep![cat=prep], 
	NP![cat=np]
	where PP?struct = {Prep?struct,NP?struct}.

VP![cat=vp] ---> 
	VP1![cat=vp], 
	PP![cat=pp]
	where VP?struct = {VP1?struct,PP?struct}.
VP![cat=vp] ---> 
	IV![cat=iv]
	where VP?struct = {IV?struct}.
VP![cat=vp] ---> 
	TV![cat=tv], 
	NP![cat=np]
	where VP?struct = {TV?struct,NP?struct}.


%% terminaler

Term![cat=Cat] ---> 
	[ Word ]
	where Term?struct = Word
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




