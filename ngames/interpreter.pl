/** <general.pl> General rule interpreter

 This script includes the definitions of common operators and of common
 predicates as dynamics and discontiguous. It also includes the predicates to
 query the rule base according to the established facts in the knowledge base
 and process their consequences into a Python-friendly format.

 @author: Nieves Montes
 */

/*** Reserved predicates for agents, participants and roles ***/

:- dynamic agent/1, role/2, participates/1, can/2, does/2.
:- discontiguous (agent)/1, (role)/2, (participates)/1, (can)/2, (does)/2.

/*** Predicate for initial conditions and compatibility ***/

:- dynamic initially/1, terminal/0, compatible/2.
:- discontiguous (initially)/1, (terminal)/0, (compatible)/2.

/*** Operators for if-then rules ***/

:- dynamic rule/4.
:- discontiguous (rule)/4.

% operators to express conditions and consequences
:- op(600, fx, if).
:- op(500, xfx, then).
:- op(400, xfx, where).
:- discontiguous (if)/1, (then)/2, (where)/2.

% :- op(300, xfy, or).
:- op(200, xfy, and).
% :- op(175, xfy, xor).
:- op(150, fx, ~). % operator for ``strong'' negation
:- dynamic and/2, (~)/1.
:- discontiguous (and)/2, (~)/1.
% :- dynamic or/2, and/2, xor/2, (~)/1.
% :- discontiguous (or)/2, (and)/2, (xor)/2, (~)/1.

% operator to express the probability of effects
:- op(350, yfx, withProb).
:- discontiguous (withProb)/2.

/*** Interpreter predicates ***/

query(A) :-
 call(A).

query(A and B) :-
 query(A),
 query(B).

% query_rule/1
% query_rule(?Rule): True if Rule is active given the current facts.
query_rule(rule(ID,Type,Priority,if Condition then Consequence
                                where Constraints)) :-
  rule(ID,Type,Priority,if Condition then Consequence where Constraints),
  maplist(query,[Condition|Constraints]).

% find_consequences/3
% find_consequences(+ID,+Type,+Threshold,-L): Find the instantitations of the
%   if-then-where rules of the Type kind, that are currently active, and
%   extract their consequences paired with their priority. Active rules with
%   priority larger than Threshold are excluded.
find_consequences(ID,Type,Threshold,L) :-
  Rule = rule(ID,Type,Priority,if _ then Consequence where _),
  findall(Priority-Consequence,query_rule(Rule),L1),
  delete_key_gt(L1,Threshold,L2),
  keysort(L2,L3),
  reverse(L3,L).

% process_consequences/3
% process_consequences(+ConseqList,+OldPartList,?NewPartList): It gets the
%   consequences of some rule type ConseqList and processes them in decreasing
%   order of priority. It returns a list NewPartList indicating the
%   consequences that hold, negated ones included. ConseqList is processed in a
%   way such that consequences that are in conflict with consequences of higher
%   precedence are discarded. It should be called with:
%   ?- find_consequences(boundary,L),process_consequences(L,[],P).
process_consequences([],OldPartList,OldPartList).
process_consequences(ConseqList,OldPartList,NewPartList) :-
  ConseqList = [_-V1|T],
  add_conseq(V1,OldPartList,IntPartList),
  process_consequences(T,IntPartList,NewPartList).


/*** predicates to find the participants, roles and actions ***/

% get_simple_consequences/4
% get_simple_consequences(+ID,+Type,+Threshold,-L): Intended to process the
%   consequences of boundary, position and choice rules. It gets the
%   consequences of the rules with ID and of Type, processess them to have
%   rules of higher priority overwrite rules of lower priority, and finally
%   deletes negated facts. It returns the result in a list of fluents.
get_simple_consequences(ID,Type,Threshold,L) :-
  find_consequences(ID,Type,Threshold,L1),
  process_consequences(L1,[],L2),
  delete(L2,~_,L).

get_participants(ID,Threshold,L) :-
  get_simple_consequences(ID,boundary,Threshold,L).

get_roles(ID,Threshold,L) :-
  get_simple_consequences(ID,position,Threshold,L).

get_actions(ID,Threshold,L) :-
  get_simple_consequences(ID,choice,Threshold,L).

/*** predicates to find the next state based on the actions performed ***/

% process_control/3
% process_control(+ID,+Threshold,-ProcessedList): process the consequences of
%   all the control rules active in the action situation, identified by ID and
%   whose priority does not exceed threshold, into the list ProcessedList,
%   which has format:
%   [Priority_1-[[[Fact_11,...],Prob_11],
%                [[Fact_12,...],Prob_12],
%                ...,
%                [[Fact_1n,...],Prob_1n]],
%    ...
%    Priority_m-[[[Fact_m1,...],Prob_11],
%                [[Fact_m2,...],Prob_12],
%                ...,
%                [[Fact_mk,...],Prob_1n]]]
%   Note: the processed consequences of activated rules are returned in
%   decreasing priority.
process_control(ID,Threshold,ProcessedList) :-
  find_consequences(ID,control,Threshold,L1),
  maplist(process_control_rule,L1,L2),
  keysort(L2,L3),
  reverse(L3,ProcessedList).

% process_control_rule/2
% process_control_rule(+(Flag-ConsequencesList),-(Flag-NewList)): process all
%   the consequences of any instantiation of a control rule into NewList, and
%   include its priority flag.
process_control_rule(Flag-List,Flag-NewList) :-
  process_rule_consequence(List,[],NewList).

% process_rule_consequence/3
% process_rule_consequence(+ConsequencesList,+OldList,-NewList): process all
%   the potential consequences of any instantiation of a control rule into the
%   list NewList.
%   Note: it is intended to be called with:
%   ?- process_rule_consequence(ConsequencesList,[],NewList).
process_rule_consequence([],OldList,OldList).
process_rule_consequence([Fact withProb Prob|T],OldList,NewList) :-
  process_one_consequence(Fact,L),
  append(OldList,[[L,Prob]],IntList),
  process_rule_consequence(T,IntList,NewList).

% process_one_consequence/2
% process_one_consequence(+Fact,-ProcessedList): process the single Fact of all
%   the potential consequences of any instantiation of a control rule, which
%   might consist of a conjunction of facts, into ProcessedList.
process_one_consequence(Fact1 and Fact2,[Fact1|T]) :-
  !,process_one_consequence(Fact2,T).
process_one_consequence(Fact,[Fact]).


/*** auxiliary predicates ***/

delete_key_gt([],_,[]) :- !.
delete_key_gt([H|T],N,L) :-
  H = Priority-_,
  Priority>N,!,
  delete_key_gt(T,N,L).
delete_key_gt([H|T],N,[H|L]) :-
  delete_key_gt(T,N,L).

add_conseq(C,Old,Old) :- member(C,Old),!.
add_conseq(C,Old,Old) :- C=..[~,F|_],member(F,Old),!.
add_conseq(C,Old,Old) :- member(~C,Old),!.
add_conseq(C,Old,[C|Old]).
