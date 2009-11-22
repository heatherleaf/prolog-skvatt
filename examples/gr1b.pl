
:- use_module(library(skvatt)).

:- features cat, struct.



%% icke-terminaler

![cat=s, struct={NP?struct,VP?struct}] ---> 
	NP![cat=np], 
	VP![cat=vp].

![cat=X, struct={XP1?struct,Conj?struct,XP2?struct}] ---> 
	XP1![cat=X], 
	Conj![cat=conj], 
	XP2![cat=X],
	{ \+ word(X, _) }.

![cat=np, struct={NP?struct,PP?struct}] ---> 
	NP![cat=np], 
	PP![cat=pp].
![cat=np, struct={PN?struct}] ---> 
	PN![cat=pn].
![cat=np, struct={Det?struct,N?struct}] ---> 
	Det![cat=det], 
	N![cat=n].

![cat=pp, struct={Prep?struct,NP?struct}] ---> 
	Prep![cat=prep], 
	NP![cat=np].

![cat=vp, struct={VP?struct,PP?struct}] ---> 
	VP![cat=vp], 
	PP![cat=pp].
![cat=vp, struct={IV?struct}] ---> 
	IV![cat=iv].
![cat=vp, struct={TV?struct,NP?struct}] ---> 
	TV![cat=tv], 
	NP![cat=np].


%% terminaler

![cat=Cat, struct=Word] ---> 
	[ Word ],
	{ word(Cat, Word) }.


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




