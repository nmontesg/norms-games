/** <module> General rule interpreter

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

:- op(300, xfy, or).
:- op(200, xfy, and).
:- op(175, xfy, xor).
:- op(150, fx, ~). % operator for ``strong'' negation
:- dynamic or/2, and/2, xor/2, (~)/1.
:- discontiguous (or)/2, (and)/2, (xor)/2, (~)/1.

% operator to express the probability of effects
:- op(350, yfx, withProb).
:- discontiguous (withProb)/2.

/*** Meta-interpreter predicates ***/

query(Q) :-
 call(Q).

query(A and B) :-
 query(A),
 query(B).

query(A or B) :-
 query(A),!
 ;
 query(B).

query(A xor B) :-
  query(A),
  query(~B),!
  ;
  query(~A),
  query(B).

% A is true ==> ~A is false
query(~A) :-
  query(A),fail.
% A is false ==> ~A is true
query(~A) :-
  \+query(A).

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


/*** predicates to find the participants in the action situation ***/

% get_participants/3
% get_participants(+ID,+Threshold,-PartList): It return the list of agents
%   PartList who are participants given the boundary rules whose priority does
%   not exceed Threshold, that apply in the action situation identified by ID.
get_participants(ID,Threshold,PartList) :-
  find_consequences(ID,boundary,Threshold,L1),
  process_consequences(L1,[],L2),
  delete(L2,~(participates(_)),L3),
  maplist(arg(1),L3,PartList).


/*** predicates to find the roles to which participants are assigned ***/

% get_roles/3
% get_role(+ID,+Threshold,-RoleList): Return a list of key-value pairs, where
%   the keys correspond to participants and values are lists of the roles that
%   the participant is assigned to, in the action situtation identified by ID,
%   and by the position rules whose priority does not exceed Threshold. It is
%   possible for a participant to have multiple roles.
%   Note: participant agents should be already asserted into the database with:
%   ?- asserta(participant(Agent)).
get_roles(ID,Threshold,RoleList) :-
  find_consequences(ID,position,Threshold,L1),
  process_consequences(L1,[],L2),
  delete(L2,~role(_,_),L3),
  process_list(L3,[],L4),
  keysort(L4,L5),
  group_pairs_by_key(L5,RoleList).


/*** predicates to find the available actions to each participant ***/

% get_actions/3
% get_action(+ID,+Threshold,-ActionList): Return a list of key-value pairs,
%   where the keys correspond to participants and the values are lists of the
%   actions available to those participants at the current state of the system,
%   in the action situtation identified by ID, and by the choice rules whose
%   priority does not exceed Threshold.
%   Note: participant agents and their assigned roles should already be
%   asserted into the system.
get_actions(ID,Threshold,ActionList) :-
  find_consequences(ID,choice,Threshold,L1),
  process_consequences(L1,[],L2),
  delete(L2,~can(_,_),L3),
  process_list(L3,[],L4),
  keysort(L4,L5),
  group_pairs_by_key(L5,ActionList).


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

add_conseq(C1,Old,Old) :- member(C1,Old),!.
add_conseq(C1,Old,Old) :- member(~C1,Old),!.
add_conseq(C1,Old,[C1|Old]).

process_list([],OldProcessedList,OldProcessedList).
process_list([H|T],OldProcessedList,NewProcessedList) :-
  H =.. [_,P,Obj],
  append(OldProcessedList,[P-Obj],IntProcessedList),
  process_list(T,IntProcessedList,NewProcessedList).
