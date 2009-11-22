
:- use_module(library(skvatt)).

:- features cat, subcat, struct.


S![cat=s] ---> NP![cat=np], VP![cat=vp] where 
	S?struct = {NP?struct,VP?struct}.

VP![cat=vp] ---> V![cat=v] where 
	VP?struct = V?struct,
	V?subcat = [].

V![cat=v] ---> V1![cat=v], XP where 
	V?struct = {V1?struct,XP?struct},
	V1?subcat = [XP|V?subcat].


PP![cat=pp] ---> P![cat=p], NP![cat=np] where
	PP?struct = {P?struct,NP?struct}.


V![cat=v] ---> [sover] where
	V?struct = {sover}, 
	V?subcat = [].
V![cat=v] ---> [kysser] where 
	V?subcat = [NP],
	NP?cat = np,
	V?struct = {kysser}.
V![cat=v] ---> [ger] where
	V?struct={ger}, 
	V?subcat=[NP,PP],
	NP?cat = np, 
	PP?cat = pp.

![cat=np, struct={pelle}] ---> [pelle].
![cat=np, struct={lisa}] ---> [lisa].
![cat=np, struct={en,bok}] ---> [en,bok].
![cat=np, struct={den,fina,flickan}] ---> [den,fina,flickan].

![cat=p, struct={till}] ---> [till].

