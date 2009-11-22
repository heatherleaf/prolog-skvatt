

:- use_module(library(skvatt)).

:- features cat, agr, num, gen.


prs(Cat, List) :- 
        parse(![cat=Cat], List).
gen(Num, Cat) :- 
        generate(Num, ![cat=Cat]).


NP![cat=np, agr=AGR] ---> 
        DET![cat=det, agr=AGR], 
        NOM![cat=nom, agr=AGR].
         
NOM![cat=nom, agr=AGR] ---> 
        N![cat=n, agr=AGR].
NOM![cat=nom, agr=AGR] ---> 
        ADJ![cat=adj, agr=AGR], 
        NOM2![cat=nom, agr=AGR].

Term ---> [Word]
	when lex(Word, Term?cat, Term?agr).


lex(Word, Cat, ![num=Num,gen=Gen]) :-
        word(Word, Cat, Num, Gen).


word( ett,   det, sg, neut ).
word( en,    det, sg, utr  ).
word( vin,   n,   sg, neut ).
word( dryck, n,   sg, utr  ).
word( gott,  adj, sg, neut ).
word( vitt,  adj, sg, neut ).
word( god,   adj, sg, utr  ).
word( vit,   adj, sg, utr  ).

