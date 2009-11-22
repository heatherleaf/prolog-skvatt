

:- use_module(library(skvatt)).

:- features cat, agr, num, gen.


prs(Cat, List) :- 
        parse(![cat=Cat], List).
gnr(Num, Cat) :- 
        generate(Num, ![cat=Cat]).


NP![cat=np, agr=AGR] ---> 
        DET![cat=det, agr=AGR], 
        NOM![cat=nom, agr=AGR].

NOM![cat=nom, agr=AGR] ---> 
        N![cat=n,agr=AGR].
NOM![cat=nom, agr=AGR] ---> 
        ADJ![cat=adj, agr=AGR], 
        NOM2![cat=nom, agr=AGR].

DET![cat=det] ---> 
        [ett]
	where (
		DET?agr = ![num=sg,gen=neut] 
	      ).
DET![cat=det] ---> 
        [en]
	where (
		DET?agr = ![num=sg,gen=utr] 
	      ).

N![cat=n] ---> 
        [vin]
	where (
		N?agr = ![num=sg,gen=neut] 
	      ).
N![cat=n] ---> 
        [dryck]
	where (
		N?agr = ![num=sg,gen=utr] 
	      ).

ADJ![cat=adj] ---> 
        [gott]
	where (
		ADJ?agr = ![num=sg,gen=neut] 
	      ).
ADJ![cat=adj] ---> 
        [vitt]
	where (
		ADJ?agr = ![num=sg,gen=neut] 
	      ).
ADJ![cat=adj] ---> 
        [god]
	where (
		ADJ?agr = ![num=sg,gen=utr] 
	      ).
ADJ![cat=adj] ---> 
        [vit]
	where (
		ADJ?agr = ![num=sg,gen=utr] 
	      ).



