
:- use_module(library(skvatt)).

:- features cat, struct.



%% icke-terminaler

![cat=s, struct={NPst,VPst}] ---> 
	![cat=np, struct=NPst], 
	![cat=vp, struct=VPst].

![cat=X, struct={XP1st,Conjst,XP2st}] ---> 
	![cat=X, struct=XP1st], 
	![cat=conj, struct=Conjst], 
	![cat=X, struct=XP2st],
	{ \+ word(X, _) }.

![cat=np, struct={NPst,PPst}] ---> 
	![cat=np, struct=NPst], 
	![cat=pp, struct=PPst].
![cat=np, struct={PNst}] ---> 
	![cat=pn, struct=PNst].
![cat=np, struct={Detst,Nst}] ---> 
	![cat=det, struct=Detst], 
	![cat=n, struct=Nst].

![cat=pp, struct={Prepst,NPst}] ---> 
	![cat=prep, struct=Prepst], 
	![cat=np, struct=NPst].

![cat=vp, struct={VPst,PPst}] ---> 
	![cat=vp, struct=VPst], 
	![cat=pp, struct=PPst].
![cat=vp, struct={IVst}] ---> 
	![cat=iv, struct=IVst].
![cat=vp, struct={TVst,NPst}] ---> 
	![cat=tv, struct=TVst], 
	![cat=np, struct=NPst].


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




