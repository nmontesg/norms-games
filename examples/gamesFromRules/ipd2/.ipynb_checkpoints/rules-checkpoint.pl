:- use_module(library(clpr)).

/*** Rules for the default situation ***/

% boundary
rule(ipd,boundary,0,if agent(A) then participates(A) where []).

% position
rule(ipd,position,0,if participates(A) then role(A,prisoner) where []).

% choice
rule(ipd,choice,0,if role(P,prisoner) then can(P,cooperate) where []).
rule(ipd,choice,0,if role(P,prisoner) then can(P,defect) where []).

% control
% P1@<P2 avoids equivalent instantiations of some control rules
rule(ipd,control,0,
if does(P1,A1) and does(P2,A2) then [rounds(M) withProb 1]
where [P1@<P2,rounds(N),{M=N+1},
member(A1,[cooperate,defect]),member(A2,[cooperate,defect])]).

rule(ipd,control,0,
if does(P1,cooperate) and does(P2,cooperate)
then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
where [P1@<P2,payoff(P1,X1),payoff(P2,X2),{Y1=X1+6},{Y2=X2+6}]).

rule(ipd,control,0,
if does(P1,defect) and does(P2,defect)
then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
where [P1@<P2,payoff(P1,X1),payoff(P2,X2),{Y1=X1+3},{Y2=X2+3}]).

rule(ipd,control,0,
if does(P1,cooperate) and does(P2,defect)
then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
where [payoff(P1,X1),payoff(P2,X2),{Y1=X1+0},{Y2=X2+9}]).


/*** rules to model enforceable agreements ***/
rule(ipd-neg,choice,1,if role(P,prisoner) then can(P,propose_commitment) where [rounds(0),findall(X,role(X,prisoner),L),random_member(P,L)]).

rule(ipd-neg,choice,1,if does(P1,propose_commitment) then [received_proposal(P2) withProb 1] where [P1\=P2]).
