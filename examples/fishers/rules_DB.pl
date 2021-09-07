/*** Default situation rules***/

% boundary
rule(fishers,boundary,0,if agent(A) then participates(A) where []).

% position
rule(fishers,position,0,if participates(A) then role(A,fisher) where []).

% choice
rule(fishers,choice,0,if role(A,fisher) then can(A,go_to_spot(S))
where [at(A,shore),fishing_spot(S)]).

rule(fishers,choice,0,if role(A,fisher) then can(A,stay)
where [at(A,S),fishing_spot(S)]).

rule(fishers,choice,0,if role(A,fisher) then can(A,leave)
where [at(A,S),fishing_spot(S)]).

% control
rule(fishers,control,0,if does(A,go_to_spot(S)) then [at(A,S) withProb 1]
where []).

rule(fishers,control,0,if does(A,leave)
then [at(A,S2) and payoff(A,X2) withProb 1]
where [at(A,S1),fishing_spot(S1),fishing_spot(S2),S1\=S2,
payoff(A,X1),{X2=X1-1}]).

rule(fishers,control,0,if does(F1,A) and does(F2,A)
then [won_fight(F1) and lost_fight(F2) withProb P1,
      won_fight(F2) and lost_fight(F1) withProb P2]
where [at(F1,S),at(F2,S),F1@<F2,fishing_spot(S),strength(F1,X1),
strength(F2,X2),{P1=X1/(X1+X2)},{P2=X2/(X1+X2)}]).


/*** First-in-time, first-in-right rules ***/
% the winner of the race must stay <==> cannot leave
rule(fishers,choice,n1,if role(A,fisher) then ~can(A,leave)
where [at(A,S),won_race(A),fishing_spot(S)]).

% the looser of the race has to leave <==> cannot stay
rule(fishers,choice,n1,if role(A,fisher) then ~can(A,stay)
where [at(A,S),won_race(B),B\=A,fishing_spot(S)]).

rule(fishers,control,n1,if does(F1,go_to_spot(S)) and does(F2,go_to_spot(S))
then [won_race(F1) withProb P1,won_race(F2) withProb P2]
where [F1@<F2,fishing_spot(S),speed(F1,X1),speed(F2,X2),{P1=X1/(X1+X2)},
{P2=X2/(X1+X2)}]).


/*** First-to-announce, first-in-right rule ***/
% pick a random participant as the announcer
rule(fishers,position,n2,if participates(A) then role(A,announcer)
where [findall(X,participates(X),L),random_member(A,L)]).

% when both fishers at shore, the announcer makes one (and only one)
% announcement
rule(fishers,choice,n2,if role(A,announcer) then can(A,announce_spot(S))
where [at(A,shore),at(B,shore),A\=B,fishing_spot(S)]).

rule(fishers,choice,n2,if role(A,announcer) then ~can(A,announce_spot(S))
where [announced(A,_),fishing_spot(S)]).

% no-one can go for a spot before the announcement is made
rule(fishers,choice,n2,if role(A,fisher) then ~can(A,go_to_spot(S))
where [\+announced(_,_),fishing_spot(S)]).

% announcement is made
rule(fishers,control,n2,if does(F,announce_spot(S))
then [announced(F,S) withProb 1] where []).

% when the announcer has gone to the announced spot, has to stay there
rule(fishers,choice,n2,if role(A,announcer) then ~can(A,leave)
where [at(A,S),fishing_spot(S),announced(A,S)]).

% if the non-announcer fisher goes to the announced spot, he has to leave
rule(fishers,choice,n2,if role(F,fisher) then ~can(F,stay)
where [at(F,S),fishing_spot(S),announced(_,S),\+role(F,announcer)]).
