
:- use_module(platt).

label(cat). label(tree). label(type). 

value(s). value(np). value(vp). value(pp). value(pn). value(n).
value(iv). value(tv). value(prep). value(conj). value(det).
value(pelle). value(lisa). value(kiosken). value(bilen).
value(en). value(kiosk). value(bil). value(i). value(och).
value(sover). value(leker). value(slår). value(kramar). 
value(phrase). value(word).

:- setflag( headcat, [cat] ).
:- setflag( leafcat, [leaf] ).
:- setflag( hidecat, [cat,type] ).

% grammatikregler

S ---> NP, VP, 
        { s(S), np(NP), vp(VP),
          S:tree = [NP:tree,VP:tree] }.

/*
XP ---> XP1, Conj, XP2, 
        { XP:cat = XP1:cat,
          XP:cat = XP2:cat,
	  phrase(XP), phrase(XP1), phrase(XP2),
          conj(Conj),
          XP:tree = [XP1:tree,Conj:tree,XP2:tree] }.
*/

NP ---> PN,
        { np(NP), pn(PN),
          NP:tree = PN:tree }.
NP ---> Det, N,
        { np(NP), det(Det), n(N),
          NP:tree = [Det:tree,N:tree] }.
NP1 ---> NP2, PP,
        { np(NP1), np(NP2), pp(PP),
          NP1:tree = [NP2:tree,PP:tree] }.

PP ---> Prep, NP,
        { pp(PP), prep(Prep), np(NP),
          PP:tree = [Prep:tree,NP:tree] }.

VP ---> IV,
        { vp(VP), iv(IV),
          VP:tree = IV:tree }.
VP ---> TV, NP,
        { vp(VP), tv(TV), np(NP),
          VP:tree = [TV:tree,NP:tree] }.
VP1 ---> VP2, PP,
        { vp(VP1), vp(VP2), pp(PP),
          VP1:tree = [VP2:tree,PP:tree] }.


% lexikon

PN --- "pelle", "nilsson",
        { pn(PN) }.
PN --- "lisa",
        { pn(PN) }.
PN --- "kiosken",
        { pn(PN) }.
PN --- "bilen",
        { pn(PN) }.

N --- "kiosk",
        { n(N) }.
N --- "bil",
        { n(N) }.

Det --- "en",
        { det(Det) }.

Prep --- "i",
        { prep(Prep) }.

IV --- "sover",
        { iv(IV) }.
IV --- "leker",
        { iv(IV) }.

TV --- "slår",
        { tv(TV) }.
TV --- "kramar",
        { tv(TV) }.

Conj --- "och",
        { conj(Conj) }.


% mallar

s(S) := S:cat=s, phrase(S).
np(NP) := NP:cat=np, phrase(NP).
vp(VP) := VP:cat=vp, phrase(VP).
pp(PP) := PP:cat=pp, phrase(PP).

pn(PN) := PN:cat=pn, word(PN).
det(Det) := Det:cat=det, word(Det).
n(N) := N:cat=n, word(N).
prep(Prep) := Prep:cat=prep, word(Prep).
iv(IV) := IV:cat=iv, word(IV).
tv(TV) := TV:cat=tv, word(TV).
conj(Conj) := Conj:cat=conj, word(Conj).

phrase(Ph) := Ph:type=phrase.
word(Wd) := Wd:type=word.

