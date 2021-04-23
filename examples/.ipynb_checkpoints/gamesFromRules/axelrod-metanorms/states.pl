/*** Domain characteristics ***/

% declare the predicates that characterize any state as dynamic
:- dynamic seen/2, payoff/2, time/1.

/*** Initial and termination conditions ***/
initially(time(0)).
initially(payoff(P,0)) :- participates(P).

% compatible/2
% compatible(+NewFact, +ListOfFacts): Succeds if the term NewFact is compatible
%   with the terms in ListOfFacts.
compatible(seen(A,_),_) :- !,\+does(A,_).
compatible(~seen(A,_),_) :- !,\+does(A,_).
compatible(time(_),L) :- !,\+member(time(_),L).
compatible(payoff(P,_),L) :- !,\+member(payoff(P,_),L).

% Following clauses are intended for ground terms of arity 0.
compatible(~F,L) :- !,\+member(F,L),\+member(~F,L).
compatible(F,L) :- !,\+member(F,L),\+member(~F,L).
