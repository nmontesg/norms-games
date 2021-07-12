:- use_module(library(clpr)).

/*** Rules for the default situation ***/

% boundary
rule(ipd,boundary,0,if agent(A) then participates(A) where []).

% position
rule(ipd,position,0,if participates(A) then role(A,prisoner) where []).

% choice
rule(ipd,choice,0,if role(P,prisoner) then can(P,A) where [play_action(A)]).

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
% one agent as proposer, the other answers to the proposal
rule(ipd,position,1,if participates(P) then role(P,proposer) where [P=alice]).
rule(ipd,position,1,if participates(P) then role(P,answerer) where [P=bob]).

% agents cannot take cooperate/defect action while negotiation is on-going
rule(ipd,choice,1,if role(P,prisoner) then ~can(P,A)
where [negotiation_on,play_action(A)]).

% proposer agent can propose one deal while the negotiation is on-going and the
% answerer has not received any proposal. Deals are tuples (S1, S2) where, if
% the deal is adopted, the proposer will only play S1 and the answerer will
% only play S2.
rule(ipd,choice,1,if role(P,proposer) then can(P,propose_deal(S1,S2))
where [negotiation_on,\+receives_proposal(_,_,_),
play_action(S1),play_action(S2)]).

% perfect communication: answerer always receives the intended proposal
rule(ipd,control,1,if does(_,propose_deal(S1,S2))
then [receives_proposal(P2,S1,S2) withProb 1] where [role(P2,answerer)]).

% when received, answerer can accept or reject the proposal
rule(ipd,choice,1,if role(P,answerer) then can(P,A)
where [negotiation_on,receives_proposal(P,_,_),negotiation_action(A)]).

% negotiation finishes after answerer accept or rejects proposal
rule(ipd,control,1,if does(_,A) then [negotiation_off withProb 1]
where [negotiation_action(A)]).

% if answerer accepts the deal, it is adopted
rule(ipd,control,1,if does(P,accept) then [deal(S1,S2) withProb 1]
where [receives_proposal(P,S1,S2)]).

% proposer and answerer are obliged to play the action of the deal if it has
% been adopted
rule(ipd,choice,1,if role(P,proposer) then ~can(P,A)
where [deal(S1,_),play_action(A),A\==S1]).

rule(ipd,choice,1,if role(P,answerer) then ~can(P,A)
where [deal(_,S2),play_action(A),A\==S2]).
