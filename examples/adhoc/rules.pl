rule(id,boundary,0,if agent(P) then participates(P) where []).

rule(id,position,0,if participates(P) then role(P,P) where []).

rule(id,choice,0,if role(P,p1) then can(P,a001) where [s0]).
rule(id,choice,0,if role(P,p1) then can(P,a011) where [s0]).

rule(id,choice,0,if role(P,p2) then can(P,a002) where [s0]).
rule(id,choice,0,if role(P,p2) then can(P,a012) where [s0]).

rule(id,choice,0,if role(P,p3) then can(P,a003) where [s0]).
rule(id,choice,0,if role(P,p3) then can(P,a013) where [s0]).
rule(id,choice,0,if role(P,p3) then can(P,a023) where [s0]).

rule(id,control,0,if does(p1,a001) and does(p2,a002) and does(p3,a003)
then [s1 withProb 1] where []).
rule(id,control,0,if does(p1,a001) and does(p2,a002) and does(p3,a013)
then [s2 withProb 1] where [s0]).
rule(id,control,0,if does(p1,a001) and does(p2,a002) and does(p3,a023)
then [s3 withProb 0.5, s4 withProb 0.5] where [s0]).

rule(id,control,0,if does(p1,a001) and does(p2,a012) and does(p3,a003)
then [s5 withProb 1] where [s0]).
rule(id,control,0,if does(p1,a001) and does(p2,a012) and does(p3,a013)
then [s6 withProb 1] where [s0]).
rule(id,control,0,if does(p1,a001) and does(p2,a012) and does(p3,a023)
then [s7 withProb 1] where [s0]).

rule(id,control,0,if does(p1,a011) and does(p2,a002) and does(p3,a003)
then [s8 withProb 1] where [s0]).
rule(id,control,0,if does(p1,a011) and does(p2,a002) and does(p3,a013)
then [s9 withProb 0.25, s10 withProb 0.75] where [s0]).
rule(id,control,0,if does(p1,a011) and does(p2,a002) and does(p3,a023)
then [s11 withProb 1] where [s0]).

rule(id,control,0,if does(p1,a011) and does(p2,a012) and does(p3,a003)
then [s12 withProb 1] where [s0]).
rule(id,control,0,if does(p1,a011) and does(p2,a012) and does(p3,a013)
then [s13 withProb 1] where [s0]).
rule(id,control,0,if does(p1,a011) and does(p2,a012) and does(p3,a023)
then [s14 withProb 1] where [s0]).
