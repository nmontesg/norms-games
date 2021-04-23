:- use_module(library(clpr)).

/*** Default situation rules***/

% boundary
rule(fishers,boundary,0,if agent(A) then participates(A) where []).

% position
rule(fishers,position,0,if participates(A) then role(A,fisher) where []).

% choice
rule(fishers,choice,0,
if role(A,fisher) then can(A,go_to_spot(S))
where [at(A,shore),fishing_spot(S)]).

rule(fishers,choice,0,
if role(A,fisher) then can(A,stay) where [at(A,S),fishing_spot(S)]).

rule(fishers,choice,0,
if role(A,fisher) then can(A,leave) where [at(A,S),fishing_spot(S)]).

% control
rule(fishers,control,0,
if does(A,go_to_spot(S)) then [at(A,S) withProb 1] where []).

rule(fishers,control,0,
if does(A,leave) then [at(A,S2) withProb 1]
where [at(A,S1),fishing_spot(S1),fishing_spot(S2),S1\=S2]).

rule(fishers,control,0,
if does(F1,A) and does(F2,A)
then [lost_fight(F1) withProb P1,
      lost_fight(F2) withProb P2]
where [at(F1,S),at(F2,S),F1@<F2,fishing_spot(S),
strength(F1,X1),strength(F2,X2),{P1=X1/(X1+X2)},{P2=X2/(X1+X2)}]).


/*** First-in-time, first-in-right rules ***/
% the looser of the race cannot stay <==> must leave
rule(fishers,choice,1,
if role(A,fisher) then ~can(A,stay)
where [at(A,S),lost_race(A),fishing_spot(S)]).

% the winner of the race has to stay <==> cannot leave
rule(fishers,choice,1,
if role(A,fisher) then ~can(A,leave)
where [at(A,S),~lost_race(A),fishing_spot(S)]).

rule(fishers,control,1,
if does(F1,go_to_spot(S)) and does(F2,go_to_spot(S))
then [lost_race(F1) withProb P1,
     lost_race(F2) withProb P2]
where [F1@<F2,fishing_spot(S),speed(F1,X1),speed(F2,X2),
{P1=X1/(X1+X2)},{P2=X2/(X1+X2)}]).


/*** First-to-announce, first-in-right rule ***/
% TODO: not adjusted to the syntax! Review and correct
rule(fishers,position,2,
if participates(A) and participates(B) then role(C,announcer)
where [A@<B,random(X),(X<0.5 -> C=A;C=B)]).

% when both fishers at shore, the announcer makes one (and only one)
% announcement
rule(fishers,choice,2,
if role(A,announcer) then can(A,announce_spot(S))
where [at(A,shore) and at(B,shore),A\=B,fishing_spot(S)]).

rule(fishers,choice,2,
if role(A,announcer) then ~can(A,announce_spot(S))
where [announced(A,_),fishing_spot(S)]).

% no-one can go for a spot before the announcement is made
rule(fishers,choice,2,
if role(A,fisher) then ~can(A,go_to_spot(S))
where [\+announced(_,_),fishing_spot(S)]).

% announcement is made
rule(fishers,control,2,
if does(F,announce_spot(S)) then [announced(F,S) withProb 1] where []
).

% if the fishers both go to the fishing spot declared by the announcer, the
% announcer is guaranteed to get there first
rule(fishers,control,2,
if does(F1,go_to_spot(S)) and does(F2,go_to_spot(S))
then [lost_race(F2) withProb 1]
where [announced(F1,S),F1\=F2]).
