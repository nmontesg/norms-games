% biophysical attributes: there are two fishing spots
fishing_spot(spot1).
fishing_spot(spot2).

:- dynamic at/2, won_fight/1, lost_fight/1, won_race/1, announced/2, payoff/2.

initially(at(F,shore)) :- role(F,fisher).
initially(payoff(F,0)) :- role(F,fisher).

terminal :-
  at(F1,S1),at(F2,S2),
  fishing_spot(S1),fishing_spot(S2),
  F1\=F2,S1\=S2.
terminal :- won_fight(_).

% material utilities at terminal outcomes
utility(F,X) :-
  terminal,
  payoff(F,Y),{X=Y+10},at(F,spot1),
  (findall(A,at(A,spot1),[F]);
  won_fight(F)).

utility(F,X) :-
  terminal,
  payoff(F,Y),{X=Y+5},at(F,spot2),
  (findall(A,at(A,spot2),[F]);
  won_fight(F)).

utility(F,X) :-
  terminal,
  payoff(F,Y),{X=Y-3},lost_fight(F).


incompatible(at(F,_),L) :- member(at(F,_),L).
incompatible(won_fight(_),L) :- member(won_fight(_),L).
incompatible(won_race(_),L) :- member(won_race(_),L).
incompatible(payoff(F,_),L) :- member(payoff(F,_),L).
