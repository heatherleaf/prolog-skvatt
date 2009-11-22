

:- use_module(library(skvatt)).

:- features cat, agr, num, gen.


prs(Cat, List) :- 
        parse(![cat=Cat], List).
gnr(Num, Cat) :- 
        generate(Num, ![cat=Cat]).


NP![cat=np] ---> 
        DET![cat=det], 
        NOM![cat=nom]
	where (
		NP?agr = DET?agr,
		NP?agr = NOM?agr 
	      ).

NOM![cat=nom] ---> 
        N![cat=n]
	where (
		NOM?agr = N?agr 
	      ).
NOM![cat=nom] ---> 
        ADJ![cat=adj], 
        NOM2![cat=nom]
	where (
		NOM?agr = NOM2?agr,
		ADJ?agr = NOM2?agr 
	      ).


DET ---> [ett]
	where (
		DET?cat = det,
		DET?agr?num = sg,
		DET?agr?gen = neut 
	      ).
DET ---> [en]
	where (
		DET?cat = det,
		DET?agr?num = sg,
		DET?agr?gen = utr 
	      ).

N ---> [vin]
	where (
		N?cat = n,
		N?agr?num = sg,
		N?agr?gen = neut 
	      ).
N ---> [dryck]
	where (
		N?cat = n,
		N?agr?num = sg,
		N?agr?gen = utr 
	      ).

ADJ ---> [gott]
	where (
		ADJ?cat = adj,
		ADJ?agr?num = sg,
		ADJ?agr?gen = neut 
	      ).
ADJ ---> [vitt]
	where (
		ADJ?cat = adj,
		ADJ?agr?num = sg,
		ADJ?agr?gen = neut 
	      ).
ADJ ---> [god]
	where (
		ADJ?cat = adj,
		ADJ?agr?num = sg,
		ADJ?agr?gen = utr 
	      ).
ADJ ---> [vit]
	where (
		ADJ?cat = adj,
		ADJ?agr?num = sg,
		ADJ?agr?gen = utr 
	      ).
