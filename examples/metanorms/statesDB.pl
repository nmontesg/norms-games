/*** Domain characteristics ***/

% declare the predicates that characterize any state as dynamic
:- dynamic seen/2, payoff/2, time/1.

/*** Initial and termination conditions ***/
initially(time(0)).
initially(payoff(P,0)) :- participates(P).

utility(A,X) :- \+terminal, payoff(A,X).

% compatible/2
% compatible(+NewFact, +ListOfFacts): Succeds if the term NewFact is compatible
%   with the terms in ListOfFacts.
incompatible(seen(A,_),_) :- does(A,_).
incompatible(~seen(A,_),_) :- does(A,_).
incompatible(time(_),L) :- member(time(_),L).
incompatible(payoff(P,_),L) :- member(payoff(P,_),L).
