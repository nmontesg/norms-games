/*** Domain characteristics ***/

% declare the predicates that characterize any state as dynamic
:- dynamic payoff/2, rounds/1, consecutiveDefections/2,
negotiation_on/0, negotiation_off/0, receives_proposal/3, deal/2.

/*** Initial and termination conditions ***/
initially(payoff(P,0)) :- role(P,prisoner).
initially(rounds(0)).
initially(consecutiveDefections(P,0)) :- role(P,prisoner),n1>0.
initially(negotiation_on) :- n3>0.

terminal :- rounds(N),N>=3.

% compatible/2
% compatible(+NewFact, +ListOfFacts): Succeeds if the term NewFact is
%   compatible with the terms in ListOfFacts.
incompatible(rounds(_),L) :- member(rounds(_),L).
incompatible(payoff(P,_),L) :- member(payoff(P,_),L).
incompatible(consecutiveDefections(P,_),L) :-
  member(consecutiveDefections(P,_),L).
  incompatible(negotiation_on,L) :- member(negotiation_off,L).
  incompatible(receives_proposal(_,_,_),L) :- member(negotiation_off,L).

play_action(A) :- member(A,[cooperate,defect]).
negotiation_action(A) :- member(A,[accept,reject]).
