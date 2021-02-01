:- use_module(library(clpr)).

/*** boundary rules ***/

rule(
  fishers,
  boundary,
  0,
  if agent(A) then participates(A) where []
).

/*** position rules ***/

rule(
  fishers,
  position,
  0,
  if participates(A) then role(A,fisher) where []
).

% first-to-announce, first-in-right rule
rule(
  fishers,
  position,
  2,
  if participates(A) and participates(B)
  then role(C,announcer)
  where [A@<B,random(X),
         (X<0.5 -> C=A
         ;
         C=B
         )]
).

/*** choice rules ***/

rule(
  fishers,
  choice,
  0,
  if role(A,fisher) and at(A,shore) then can(A,go_to_spot(S))
  where [fishing_spot(S)]
).

rule(
  fishers,
  choice,
  0,
  if role(A,fisher) and at(A,S) then can(A,stay) where [fishing_spot(S)]
).

rule(
  fishers,
  choice,
  0,
  if role(A,fisher) and at(A,S) then can(A,leave) where [fishing_spot(S)]
).

% first-in-time, first-in-right rules
% the looser of the race cannot stay <==> must leave
rule(
  fishers,
  choice,
  1,
  if role(A,fisher) and at(A,S) and lost_race(A) then ~can(A,stay)
  where [fishing_spot(S)]
).
% the winner of the race has to stay <==> cannot leave
rule(
  fishers,
  choice,
  1,
  if role(A,fisher) and at(A,S) and ~lost_race(A) then ~can(A,leave)
  where [fishing_spot(S)]
).

% first-to-announce, first-in-right rules
% when both fishers at shore, the announcer makes one (and only one)
% announcement
rule(
  fishers,
  choice,
  2,
  if role(A,announcer) and at(A,shore) and at(B,shore)
  then can(A,announce_spot(S))
  where [fishing_spot(S),A\=B]
).
rule(
  fishers,
  choice,
  2,
  if role(A,announcer) and announced(A,_)
  then ~can(A,announce_spot(S))
  where [fishing_spot(S)]
).

% no-one can go for a spot before the announcement is made
rule(
  fishers,
  choice,
  2,
  if role(A,fisher) and ~announced(_,_)
  then ~can(A,go_to_spot(S))
  where [fishing_spot(S)]
).


/*** control rules ***/

rule(
  fishers,
  control,
  0,
  if does(A,go_to_spot(S)) then [at(A,S) withProb 1] where []
).

rule(
  fishers,
  control,
  0,
  if does(A,leave) and at(A,S1) then [at(A,S2) withProb 1]
  where [fishing_spot(S2),S1\=S2]
).

rule(
  fishers,
  control,
  0,
  if at(F1,S) and at(F2,S) and does(F1,A) and does(F2,A)
  then [lost_fight(F1) withProb P1,
        lost_fight(F2) withProb P2]
  where [F1@<F2,fishing_spot(S),
         strength(F1,X1),strength(F2,X2),
         {P1=X1/(X1+X2)},{P2=X2/(X1+X2)}]
).

% first-in-time, first-in-right rule
rule(
  fishers,
  control,
  1,
  if does(F1,go_to_spot(S)) and does(F2,go_to_spot(S))
  then [lost_race(F1) withProb P1,
        lost_race(F2) withProb P2]
  where [F1@<F2,fishing_spot(S),
         speed(F1,X1),speed(F2,X2),
         {P1=X1/(X1+X2)},{P2=X2/(X1+X2)}]
).

% first-to-announce, first-in-right rule
% announcement is made
rule(
  fishers,
  control,
  2,
  if does(F,announce_spot(S)) then [announced(F,S) withProb 1] where []
).
% if the fishers both go to the fishing spot declared by the announcer, the
% announcer is guaranteed to get there first
rule(
  fishers,
  control,
  2,
  if announced(F1,S) and does(F1,go_to_spot(S)) and does(F2,go_to_spot(S))
  then [lost_race(F2) withProb 1]
  where [F1\=F2]
).
