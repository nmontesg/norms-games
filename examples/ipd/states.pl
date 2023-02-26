/*** Domain characteristics ***/

% declare the predicates that characterize any state as dynamic
:- dynamic payoff/2, rounds/1, consecutiveDefections/2.

% , negotiation_on/0, negotiation_off/0, receives_proposal/3, deal/2.

/*** Initial and termination conditions ***/
initially(payoff(P,0)) :- role(P,prisoner).
initially(rounds(0)).
initially(consecutiveDefections(P,0)) :- role(P,prisoner).

terminal :- rounds(N),N>=3.

% incompatible/2
% incompatible(+NewFact, +ListOfFacts): Succeeds if the term NewFact
%   is incompatible with the terms in ListOfFacts.
incompatible(rounds(_),L) :- member(rounds(_),L).
incompatible(payoff(P,_),L) :- member(payoff(P,_),L).
incompatible(consecutiveDefections(P,_),L) :-
  member(consecutiveDefections(P,_),L).
