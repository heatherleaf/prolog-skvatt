
:- use_module(library(skvatt)).

:- features cat, subcat, struct.

S ---> NP, VP where
	S?cat = s, 
	NP?cat = np, 
	VP?cat = vp, 
	S?struct = {NP?struct,VP?struct}.

VP ---> V where
	VP?cat = vp, 
	V?cat = v, 
	VP?struct = V?struct,
	V?subcat = [].

V ---> V1, XP where
	V?cat = v, 
	V1?cat = v, 
	V?struct = {V1?struct,XP?struct},
	V1?subcat = [XP|V?subcat].

PP ---> P, NP where
	PP?cat = pp, 
	P?cat = p, 
	NP?cat = np,
	PP?struct = {P?struct,NP?struct}.

V ---> [sover] where
	V?cat = v,
	V?struct = {sover}, 
	V?subcat = [].

V ---> [kysser] where
	V?cat = v, 
	V?subcat = [NP],
	NP?cat = np,
	V?struct = {kysser}.

V ---> [ger] where
	V?cat = v,
	V?struct={ger}, 
	V?subcat=[NP,PP],
	NP?cat = np, 
	PP?cat = pp.

![cat=np, struct={pelle}] ---> [pelle].
![cat=np, struct={lisa}] ---> [lisa].
![cat=np, struct={en,bok}] ---> [en,bok].
![cat=np, struct={den,fina,flickan}] ---> [den,fina,flickan].

![cat=p, struct={till}] ---> [till].

