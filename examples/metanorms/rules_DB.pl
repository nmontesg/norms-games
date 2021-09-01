/*** Default rules ***/

% boundary
rule(metanorms,boundary,0,if agent(A) then participates(A) where []).

% position
rule(metanorms,position,0,
if participates(A) then role(A,individual) where [A=i]).

rule(metanorms,position,0,
if participates(A) then role(A,monitor) where [A=j]).

% choice
rule(metanorms,choice,0,
if role(A,individual) then can(A,defect) where [time(0)]).

rule(metanorms,choice,0,
if role(A,individual) then can(A,~defect) where [time(0)]).

rule(metanorms,choice,0,
if role(P2,monitor) then can(P2,sanction(P1)) where [seen(P2,P1)]).

rule(metanorms,choice,0,
if role(P2,monitor) then can(P2,~sanction(P1)) where [seen(P2,P1)]).

% control
rule(metanorms,control,0,
if does(_,_) then [time(M) withProb 1] where [time(N),{M=N+1}]).

rule(metanorms,control,0,
if does(P,defect) then [payoff(P,Y) withProb 1] where [payoff(P,X),{Y=X+3}]).

rule(metanorms,control,0,
if does(P1,defect) then [payoff(P2,Y) withProb 1]
where [payoff(P2,X),P2\=P1,{Y=X-1}]).

rule(metanorms,control,0,
if does(P1,defect) then [seen(P2,P1) withProb 0.6, ~seen(P2,P1) withProb 0.4]
where [role(P1,individual),role(P2,monitor)]).

rule(metanorms,control,0,
if does(P2,sanction(P1)) then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
where [payoff(P1,X1),payoff(P2,X2),{Y1=X1-9},{Y2=X2-2}]).


/*** Introduce metamonitor ***/
rule(metanorms,position,n1,
if participates(A) then role(A,metamonitor) where [A=k]).

rule(metanorms,choice,n1,
if role(P2,metamonitor) then can(P2,sanction(P1)) where [seen(P2,P1)]).

rule(metanorms,choice,n1,
if role(P2,metamonitor) then can(P2,~sanction(P1)) where [seen(P2,P1)]).

rule(metanorms,control,n1,
if does(P1,~sanction(_))
then [seen(P2,P1) withProb 0.6, ~seen(P2,P1) withProb 0.4]
where [role(P1,monitor),role(P2,metamonitor)]).
