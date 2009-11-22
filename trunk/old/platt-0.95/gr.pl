
:- use_module(platt).

label(cat). label(tree). label(type). label(leaf).

value(s). value(np). value(vp). value(pp). value(pn). value(n).
value(iv). value(tv). value(prep). value(conj). value(det).
value(pelle). value(lisa). value(kiosken). value(bilen).
value(en). value(kiosk). value(bil). value(i). value(och).
value(sover). value(leker). value(slår). value(kramar). 
value(phrase). value(word).

:- setflag( catlabel, cat ).
:- setflag( headlabel, [cat] ).
:- setflag( leaflabel, [leaf] ).
:- setflag( hidelabel, [cat,type] ).
:- setflag( parsetrees, on ).

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

PN --- "pelle",
        { pn(PN), PN:tree = pelle }.
PN --- "lisa",
        { pn(PN), PN:tree = lisa }.
PN --- "kiosken",
        { pn(PN), PN:tree = kiosken }.
PN --- "bilen",
        { pn(PN), PN:tree = bilen }.

N --- "kiosk",
        { n(N), N:tree = kiosk }.
N --- "bil",
        { n(N), N:tree = bil }.

Det --- "en",
        { det(Det), Det:tree = en }.

Prep --- "i",
        { prep(Prep), Prep:tree = i }.

IV --- "sover",
        { iv(IV), IV:tree = sover }.
IV --- "leker",
        { iv(IV), IV:tree = leker }.

TV --- "slår",
        { tv(TV), TV:tree = slår }.
TV --- "kramar",
        { tv(TV), TV:tree = kramar }.

Conj --- "och",
        { conj(Conj), Conj:tree = och }.


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

