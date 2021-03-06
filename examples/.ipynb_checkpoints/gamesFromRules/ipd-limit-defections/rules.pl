:- use_module(library(clpr)).

/*** Boundary rules ***/
% by default all agents participate
rule(
  ipd,
  boundary,
  0,
  if agent(A) then participates(A) where []
).


/*** Position rules ***/
% by default, all participants are prisoners
rule(
  ipd,
  position,
  0,
  if participates(A) then role(A,prisoner) where []
).


/*** Choice rules ***/
% by default, both prisoners can defect or cooperate whenever
rule(
  ipd,
  choice,
  0,
  if role(P,prisoner) then can(P,cooperate) where []
).

rule(
  ipd,
  choice,
  0,
  if role(P,prisoner) then can(P,defect) where []
).

% defection is not allowed if some agent has already defected twice in a row
rule(
  ipd,
  choice,
  1,
  if role(P,prisoner) and consecutiveDefections(P,N) then ~can(P,defect)
  where [N>=2]
).


/*** Control rules ***/
% P1@<P2 avoids equivalent instantiations of some control rules
rule(
  ipd,
  control,
  0,
  if rounds(N) and does(P1,_) and does(P2,_)
  then [rounds(M) withProb 1]
  where [P1@<P2,{M=N+1}]
).

rule(
  ipd,
  control,
  0,
  if does(P1,cooperate) and does(P2,cooperate)
  then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
  where [P1@<P2,payoff(P1,X1),payoff(P2,X2),{Y1=X1+6},{Y2=X2+6}]
).

rule(
  ipd,
  control,
  0,
  if does(P1,defect) and does(P2,defect)
  then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
  where [P1@<P2,payoff(P1,X1),payoff(P2,X2),{Y1=X1+3},{Y2=X2+3}]
).

rule(
  ipd,
  control,
  0,
  if does(P1,cooperate) and does(P2,defect)
  then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
  where [payoff(P1,X1),payoff(P2,X2),{Y1=X1+0},{Y2=X2+9}]
).

rule(
  ipd,
  control,
  1,
  if does(P,defect) then [consecutiveDefections(P,M) withProb 1]
  where [consecutiveDefections(P,N),{M=N+1}]
).

rule(
  ipd,
  control,
  1,
  if does(P,cooperate) then [consecutiveDefections(P,0) withProb 1] where []
).
