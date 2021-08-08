% Este es el archivo donde se describen las normas del juego. Este sí que
% lo tendrás que modificar en algunos puntos.
%
% Primero se declaran las normas del juego por defecto, que tienen una
% prioridad (tercer argumento de rule) igual a 0. Normas adicionales se
% escriben con prioridades 1, 2,... sucesivamente.

:- use_module(library(clpfd)).

/*** Rules for the default situation ***/

% boundary
% con esta norma se permite participar en el juego a todos los agentes.
rule(ipd,boundary,0,if agent(A) then participates(A) where []).

% position
% con esta norma todos los agentes se asignan el rol de prisionero.
rule(ipd,position,0,if participates(A) then role(A,prisoner) where []).

% choice
% con esta norma se declaran qué acciones pueden tomar los prisioneros
rule(ipd,choice,0,if role(P,prisoner) then can(P,A) where [play_action(A)]).

% control
% P1@<P2 avoids equivalent instantiations of some control rules
% con estas normas se describe qué efectos tienen las acciones que toman los
% prisioneros

% esta norma declara que cuando los dos prisioneros toman una acción, el
% contador del número de rondas aumenta en una unidad.
rule(ipd,control,0,
if does(P1,A1) and does(P2,A2) then [rounds(M) withProb 1]
where [P1@<P2,rounds(N),M#=N+1,play_action(A1),play_action(A2)]).

% las siguientes normas describen los payoffs "por defecto" (matriz de
% recompensas A). Usa la siguiente notació:
%   A1: recompensa por cooperación mutua
%   A2: recompensa por deserción mutua
%   A3: recompensa por ser traicionado
%   A4: recompensa por traicionar

% esta norma describe cómo incrementan las recompensas si ambos prisioneros
% cooperan
rule(ipd,control,0,
if does(P1,cooperate) and does(P2,cooperate)
then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
where [P1@<P2,payoff(P1,X1),payoff(P2,X2),Y1#=X1+A1,Y2#=X2+A1,
A1#=15]).

% esta norma describe cómo incrementan las recompensas si ambos prisioneros
% no cooperan
rule(ipd,control,0,
if does(P1,defect) and does(P2,defect)
then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
where [P1@<P2,payoff(P1,X1),payoff(P2,X2),Y1#=X1+A2,Y2#=X2+A2,
A2#=10]).

% esta norma describe cómo incrementan las recompensas si un prisionero
% coopera y el otro no
rule(ipd,control,0,
if does(P1,cooperate) and does(P2,defect)
then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
where [payoff(P1,X1),payoff(P2,X2),Y1#=X1+A3,Y2#=X2+A4,
A3#=5,A4#=20]).


% NORMAS ADICIONALES
% se introduce la matriz B de recompensas cada N rondas
% (aquí N está fijado en 2). La notación B1, B2, B3, B4 es equivalente a la
% de la matriz A.

rule(ipd,control,1,
if does(P1,cooperate) and does(P2,cooperate)
then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
where [P1@<P2,payoff(P1,X1),payoff(P2,X2),Y1#=X1+B1,Y2#=X2+B1,
B1#=1,
rounds(N),0#=N mod 2]). % <- en esta línea puedes cambiar cada cuántas rondas
% se aplica la matriz B en vez de la A. Debes hacerlo en todas las normas
% adicionales.

rule(ipd,control,1,
if does(P1,defect) and does(P2,defect)
then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
where [P1@<P2,payoff(P1,X1),payoff(P2,X2),Y1#=X1+B2,Y2#=X2+B2,
B2#=2,
rounds(N),0#=N mod 2]).

rule(ipd,control,1,
if does(P1,cooperate) and does(P2,defect)
then [payoff(P1,Y1) and payoff(P2,Y2) withProb 1]
where [payoff(P1,X1),payoff(P2,X2),Y1#=X1+B3,Y2#=X2+B4,
B3#=3,B4#=4,
rounds(N),0#=N mod 2]).
