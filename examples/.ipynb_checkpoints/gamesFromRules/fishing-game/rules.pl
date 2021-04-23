:- use_module(library(clpr)).

/*** boundary rules ***/
rule(fishers,boundary,0,if agent(A) then participates(A)).

/*** position rules ***/
rule(fishers,position,0,if participates(A) then role(A,fisher)).

% first-to-announce, first-in-right rule
rule(fishers,position,2,
if participates(A) and participates(B) then role(C,announcer)) :-
    A@<B,random(X),(X<0.5 -> C=A;C=B).

/*** choice rules ***/
rule(fishers,choice,0,if role(A,fisher) then can(A,go_to_spot(S))) :-
  at(A,shore),fishing_spot(S).

rule(fishers,choice,0,if role(A,fisher) then can(A,stay)) :-
  at(A,S),fishing_spot(S).

rule(fishers,choice,0,if role(A,fisher) then can(A,leave)) :-
  at(A,S),fishing_spot(S).

% first-in-time, first-in-right rules
% the looser of the race cannot stay <==> must leave
rule(fishers,choice,1,if role(A,fisher) then ~can(A,stay)) :-
  at(A,S),lost_race(A),fishing_spot(S).

% the winner of the race has to stay <==> cannot leave
rule(fishers,choice,1,if role(A,fisher) then ~can(A,leave)) :-
  at(A,S),\+lost_race(A),fishing_spot(S).

% first-to-announce, first-in-right rules
% when both fishers at shore, the announcer makes one (and only one)
% announcement
rule(fishers,choice,2,if role(A,announcer) then can(A,announce_spot(S))) :-
  at(A,shore),at(B,shore),A\=B,fishing_spot(S).

rule(fishers,choice,2,if role(A,announcer) then ~can(A,announce_spot(S))) :-
  announced(A,_),fishing_spot(S).

% no-one can go for a spot before the announcement is made
rule(fishers,choice,2,if role(A,fisher) then ~can(A,go_to_spot(S))) :-
  \+announced(_,_),fishing_spot(S).

/*** control rules ***/
rule(fishers,control,0,if does(A,go_to_spot(S)) then [at(A,S) withProb 1]).

rule(fishers,control,0,if does(A,leave) then [at(A,S2) withProb 1]) :-
  at(A,S1),fishing_spot(S1),fishing_spot(S2),S1\=S2.

rule(fishers,control,0,
if does(F1,A) and does(F2,A)
then [lost_fight(F1) withProb P1,lost_fight(F2) withProb P2]) :-
  at(F1,S),at(F2,S),F1@<F2,fishing_spot(S),
  strength(F1,X1),strength(F2,X2),{P1=X1/(X1+X2)},{P2=X2/(X1+X2)}.

% first-in-time, first-in-right rule
rule(fishers,control,1,
if does(F1,go_to_spot(S)) and does(F2,go_to_spot(S))
then [lost_race(F1) withProb P1,lost_race(F2) withProb P2]) :-
  F1@<F2,fishing_spot(S),
  speed(F1,X1),speed(F2,X2),{P1=X1/(X1+X2)},{P2=X2/(X1+X2)}.

% first-to-announce, first-in-right rule
% announcement is made
rule(fishers,control,2,
if does(F,announce_spot(S)) then [announced(F,S) withProb 1]).

% if the fishers both go to the fishing spot declared by the announcer, the
% announcer is guaranteed to get there first
rule(fishers,control,2,
if does(F1,go_to_spot(S)) and does(F2,go_to_spot(S))
then [lost_race(F2) withProb 1]) :-
  announced(F1,S),F1\=F2.
