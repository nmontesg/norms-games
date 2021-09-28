/*** Default situation rules ***/

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
payoff(A,X1),C is -2,{X2=X1+C}]).

rule(fishers,control,0,if does(F1,A) and does(F2,A)
then [
    won_fight(F1) and payoff(F1,U11) and lost_fight(F2) and payoff(F2,U21) withProb P1,
    won_fight(F2) and payoff(F2,U22) and lost_fight(F1) and payoff(F1,U12) withProb P2
    ]
where [
    at(F1,S),at(F2,S),F1@<F2,fishing_spot(S),
    strength(F1,X1),strength(F2,X2),{P1=X1/(X1+X2)},{P2=X2/(X1+X2)},
    productivity(S,H),D is -6,payoff(F1,U1),payoff(F2,U2),
    {U11=U1+H},{U12=U1+D},{U21=U2+D},{U22=U2+H}
    ]
).

% payoff
rule(fishers,payoff,0,if role(F,fisher) then payoff(F,Y)
where [
    at(F,S),fishing_spot(S),findall(A,at(A,S),[F]),
    productivity(S,H),payoff(F,X),{Y=X+H}
    ]
).
