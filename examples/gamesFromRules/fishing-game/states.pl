% biophysical attributes: there are two fishing spots
fishing_spot(spot1).
fishing_spot(spot2).

:- dynamic at/2, lost_fight/1, lost_race/1, won_race/1, announced/2.

initially(at(F,shore)) :- role(F,fisher).

terminal :- at(F1,S1),at(F2,S2),F1\=F2,S1\=S2.
terminal :- lost_fight(_).

compatible(at(F,_),L) :- !,\+member(at(F,_),L).
compatible(lost_fight(F),L) :- !,\+member(lost_fight(F),L).
compatible(lost_race(F),L) :- !,\+member(lost_race(F),L).
compatible(won_race(F),L) :- !,\+member(won_race(F),L).
compatible(_,[]).
