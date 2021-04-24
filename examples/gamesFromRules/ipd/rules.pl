:- use_module(library(clpr)).

/*** Rules for the default situation ***/

% boundary
rule(ipd,boundary,0,if agent(A) then participates(A) where []).

%position
rule(ipd,position,0,if participates(A) then role(A,prisoner) where []).

% choice
rule(ipd,choice,0,if role(P,prisoner) then can(P,cooperate) where []).
rule(ipd,choice,0,if role(P,prisoner) then can(P,defect) where []).

% control
% P1@<P2 avoids equivalent instantiations of some control rules
rule(ipd,control,0,
if does(P1,_) and does(P2,_) then [rounds(M) withProb 1]
where [P1@<P2,rounds(N),{M=N+1}]).

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


/*** Rules to limit the number of consecutive defections ***/
rule(ipd,choice,1,
if role(P,prisoner) then ~can(P,defect)
where [consecutiveDefections(P,N),N>=2]).

rule(ipd,control,1,
if does(P,defect) then [consecutiveDefections(P,M) withProb 1]
where [consecutiveDefections(P,N),{M=N+1}]).

rule(ipd,control,1,
if does(P,cooperate) then [consecutiveDefections(P,0) withProb 1] where []).


/*** Rules to ban mutual defection ***/
rule(ipd,choice,2,if role(P,prisoner) then can(P,defect) where []).

rule(ipd,control,2,
if does(P1,defect) and does(P2,defect)
then [payoff(P1,Y11) and payoff(P2,Y12) withProb 0.5,
      payoff(P1,Y21) and payoff(P2,Y22) withProb 0.5]
where [P1@<P2,payoff(P1,X1),payoff(P2,X2),{Y11=X1+0},{Y12=X2+9},{Y21=X1+9},
{Y22=X2+0}]).
