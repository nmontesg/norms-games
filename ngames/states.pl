dynamic :- payoff/2, rounds/1.


initially(payoff(P,0)) :- role(P,prisoner).
initially(rounds(P,0)).

terminal :- rounds(N),N>=1.

compatible(payoff(P,_),L) :-
  !,\+member(payoff(P,_),L).
compatible(rounds(_),L) :-
  !,\+member(rounds(_),L).
compatible(_,[]).
