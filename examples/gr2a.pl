

:- use_module(library(skvatt)).


:- features cat, subcat, struct.


![cat=s, struct={NPst,VPst}] --->
	![cat=np, struct=NPst], 
	![cat=vp, struct=VPst].

![cat=vp, struct=Vst] ---> 
	![cat=v, struct=Vst, subcat=[]].

![cat=v, subcat=Vsub, struct={Vst,XPst}] ---> 
	![cat=v, struct=Vst, subcat=[XP|Vsub]], 
	XP![struct=XPst].


![cat=pp, struct={Pst,NPst}] ---> 
	![cat=p, struct=Pst], 
	![cat=np, struct=NPst].


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

