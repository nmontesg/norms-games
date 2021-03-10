
:- dynamic payoff/2, rounds/1, consecutiveDefections/2.

% compatible/2
% compatible(+NewFact, +ListOfFacts): Succeds if the term NewFact is compatible
%   with the terms in ListOfFacts.
compatible(payoff(P,_),L) :-
  !,\+member(payoff(P,_),L).
compatible(rounds(_),L) :-
  !,\+member(rounds(_),L).

% Anything is compatible if no facts have been introduced yet
compatible(_,[]).

initially(payoff(P,0)) :- role(P,prisoner).
initially(rounds(0)).

terminal :- rounds(N),N>=3.
