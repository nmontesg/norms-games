% En este archivo se declaran cosas como qué predicados definen un estado
% del juego, las conditiones iniciales y terminales, entre otros.
% Este archivo no lo tienes que modificar.

/*** Domain characteristics ***/

% declare the predicates that characterize any state as dynamic
:- dynamic payoff/2, rounds/1.

/*** Initial and termination conditions ***/
initially(payoff(P,0)) :- role(P,prisoner).
initially(rounds(1)).

terminal :- rounds(N),N>=4. % <-- aquí puedes modificar el número de rondas total

% compatible/2
% compatible(+NewFact, +ListOfFacts): Succeeds if the term NewFact is
%   compatible with the terms in ListOfFacts.
incompatible(rounds(_),L) :- member(rounds(_),L).
incompatible(payoff(P,_),L) :- member(payoff(P,_),L).

play_action(A) :- member(A,[cooperate,defect]).

modulo(Dividend,Divisor,Mod) :-
  X is integer(Dividend),
  Y is integer(Divisor),
  divmod(X,Y,_,Mod).
