
:- use_module(library(skvatt)).

:- features cat, subcat, struct.


![cat=s, struct={NP?struct,VP?struct}] --->
	NP![cat=np], 
	VP![cat=vp].

![cat=vp, struct=V?struct] ---> 
	V![cat=v, subcat=[]].

V![cat=v, struct={V1?struct,XP?struct}] ---> 
	V1![cat=v, subcat=[XP|V?subcat]], 
	XP.


![cat=pp, struct={P?struct,NP?struct}] ---> 
	P![cat=p], 
	NP![cat=np].


![cat=v, struct={sover}, subcat=[]] ---> [sover].
![cat=v, struct={kysser}, subcat=[NP]] ---> [kysser]
	where NP?cat = np.
![cat=v, struct={ger}, subcat=[NP,PP]] ---> [ger]
	where NP?cat = np, PP?cat = pp.

![cat=np, struct={pelle}] ---> [pelle].
![cat=np, struct={lisa}] ---> [lisa].
![cat=np, struct={en,bok}] ---> [en,bok].
![cat=np, struct={den,fina,flickan}] ---> [den,fina,flickan].

![cat=p, struct={till}] ---> [till].

