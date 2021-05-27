/** <general.pl> General rule interpreter

 This script includes the definitions of common operators and of common
 predicates as dynamics and discontiguous. It also includes the predicates to
 query the rule base according to the established facts in the knowledge base
 and process their consequences into a Python-friendly format.

 @author: Nieves Montes
 */

:- use_module(library(clpr)).

/*** Reserved predicates for agents, participants and roles ***/

:- dynamic agent/1, role/2, participates/1, can/2, does/2.
:- discontiguous (agent)/1, (role)/2, (participates)/1, (can)/2, (does)/2.

/*** Predicate for initial conditions and compatibility ***/

:- dynamic initially/1, terminal/0, incompatible/2.
:- discontiguous (initially)/1, (terminal)/0, (incompatible)/2.

/*** Operators for if-then rules ***/

:- dynamic rule/4.
:- discontiguous (rule)/4.

% operators to express conditions and consequences
:- op(600, fx, if).
:- op(500, xfx, then).
:- op(400, xfx, where).
:- discontiguous (if)/1, (then)/2, (where)/2.

:- op(200, xfy, and).
:- op(150, fx, ~). % overwriting operator
:- dynamic and/2, (~)/1.
:- discontiguous (and)/2, (~)/1.

% operator to express the probability of effects
:- op(350, yfx, withProb).
:- discontiguous (withProb)/2.

%%-----------------------------------------------------------------------------

/*** COMMON INTERPRETER PREDICATES ***/

query(A) :-
 call(A).

query(A and B) :-
 query(A),
 query(B).

% query_rule/1
% query_rule(?Rule): True if Rule is active given the current state of the
%   system.
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

% delete_key_gt: auxiliary predicate to delete consequences whose priority is
%   over some threshold.
delete_key_gt([],_,[]) :- !.
delete_key_gt([H|T],N,L) :-
  H = Priority-_,
  Priority>N,!,
  delete_key_gt(T,N,L).
delete_key_gt([H|T],N,[H|L]) :-
  delete_key_gt(T,N,L).

%%-----------------------------------------------------------------------------

/*** PROCESS SIMPLE RULES ***/

/*** Predicates to find the participants, roles and actions ***/

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

add_conseq(C,Old,Old) :- member(C,Old),!.
add_conseq(C,Old,Old) :- member(~C,Old),!.
add_conseq(C,Old,[C|Old]).

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

%%-----------------------------------------------------------------------------

/*** PROCESSING CONTROL RULES ***/

/*** predicates to find whether a triggered control rule is compatible
with the potential next states already established ***/

% control_conseq_fact_incompatible/2
% control_conseq_fact_incompatible(+Fact,+S): Checks whether a single fact is
%   incompatible with the set of facts in S.
control_conseq_fact_incompatible(_,[]) :- !.
control_conseq_fact_incompatible(F,[S1|S2]) :-
  incompatible(F,S1),
  control_conseq_fact_incompatible(F,S2).

% control_conseq_incompatible/2
% control_conseq_incompatible(+Facts,+S): Checks whether the Facts that make up
%   a joint consequence statement of an active control rule are incompatible
%   with the next states already derived in S.
control_conseq_incompatible(F1 and F2,S) :-
  control_conseq_fact_incompatible(F1,S),
  control_conseq_incompatible(F2,S).
control_conseq_incompatible(F,S) :-
  control_conseq_fact_incompatible(F,S).

% control_rule_incompatible/2
% control_rule_incompatible(+Conseqs,+S): Check whether the list of
%   Conseqs of an active control rule is incompatible with the next states
%   already derived in S.
control_rule_incompatible([],_).
control_rule_incompatible([C withProb _|T],S) :-
  control_conseq_incompatible(C,S),
  control_rule_incompatible(T,S).


/*** predicates to add the consequences of a single rule into the set of
potential next states ***/

% add_rule_conseqs_to_next_states/7
% add_rule_conseqs_to_next_states(+Conseqs,+S,+P,+OldNextS,+OldNextP,
%   -NewNextS,-NewNextP): Given the Conseqs of an acive control rule and the
%   potential next states being derived in S and their probabilities in P,
%
add_rule_conseqs_to_next_states([],_,_,OldNextStates,OldNextP,OldNextStates,
OldNextP).
add_rule_conseqs_to_next_states([C withProb X|T],S,P,OldNextStates,OldNextP,
NewNextStates,NewNextP) :-
  add_joint_conseqs_to_next_states(C withProb X,S,P,NewS,NewP),
  append(NewS,OldNextStates,IntNextStates),
  append(NewP,OldNextP,IntNextP),
  add_rule_conseqs_to_next_states(T,S,P,IntNextStates,IntNextP,NewNextStates,
  NewNextP).

% add_joint_conseqs_to_next_states/5
% add_joint_conseqs_to_next_states(+C withProb X,+OldNextStates,+OldProb,
%   -NewNextStates,-NewProb): Given a consequence C with probability P derived
%   from an activated control rule, update the list of OldNextStates and their
%   probabilities OldProb into the updated NewNextStates and NewProb.
add_joint_conseqs_to_next_states(_,[],[],[],[]).
add_joint_conseqs_to_next_states(C withProb X,[OldS1|OldS2],[OldP1|OldP2],
[NewS1|NewS2],[NewP1|NewP2]) :-
  add_joint_conseqs_to_single_state(C withProb X,OldS1,OldP1,NewS1,NewP1),
  add_joint_conseqs_to_next_states(C withProb X,OldS2,OldP2,NewS2,NewP2).

% add_joint_conseqs_to_single_state/5
% add_joint_conseqs_to_single_state(+C withProb X,+State,+Prob,
%   -NewState,-NewProb): Takes one consequence C withProb X from an activated
%   control rule and an updated state list with probability Prob and updates
%   the list of state facts and the probability.
add_joint_conseqs_to_single_state(C withProb X,State,Prob,NewState,NewProb) :-
  joint_conseqs_to_list([],C,L),
  append(State,L,NewState),
  NewProb is X*Prob.

% joint_conseqs_to_list: auxiliary. Turns a joint consequence statement (facts
%   joined by the ``and'' operator) and into a list of facts.
joint_conseqs_to_list(Old,C1 and C2,New) :-
  !,joint_conseqs_to_list([C1|Old],C2,New).
joint_conseqs_to_list(Old,C1,[C1|Old]).


/*** predicates to drag the compatible facts from the pre-transition state
to the potential next states ***/

% drag_compatible_fact/2
% drag_compatible_fact(+PreTranFact,+PostTranState,-UpdatedPostTransState):
%   If compatible, update a provisional next state with a fluent from the pre-
%   transition state.
drag_compatible_fact(PreTranFact,NewState,NewState) :-
  incompatible(PreTranFact,NewState),!.
drag_compatible_fact(PreTranFact,NewState,[PreTranFact|NewState]).

% drag_compatible_state/3
% drag_compatible_fact(+PreTranFact,+PostTranState,-UpdatedPostTransState):
%   Update the facts in PostTranState with the compatible facts from
%   PreTranState, and return UpdatedPostTransState.
drag_compatible_state([],NewState,NewState).
drag_compatible_state([F|T],NewState,UpdatedNewState) :-
  drag_compatible_fact(F,NewState,Int),
  drag_compatible_state(T,Int,UpdatedNewState).

% update_all_new_states/3
% update_all_new_states(+PreTranState,+PostTranStates,-UpdatedPostTransStates):
%   Update the PostTranStates with the compatible facts from the PreTranState
%   to return the UpdatedPostTransStates.
update_all_new_states(_,[],[]).
update_all_new_states(PreTranState,[S1|S2],[NewS1|NewS2]) :-
  drag_compatible_state(PreTranState,S1,NewS1),
  update_all_new_states(PreTranState,S2,NewS2).


/*** predicates to find the next state based on the actions performed ***/

% add_control_rules/5
% add_control_rules(+RuleConseqs,+OldNextS,+OldNextP,-NewNextS,-NewNextP):
%   Given the priority-consequences pairs of the activated control rules in
%   RuleConseqs, append them to the provisional OldNextS with their unadapted
%   probabilities in OldNextP. If the rule is compatible with the facts
%   already established, add the consequences into NewNextS and update the
%   probabilities into NewNextP.
add_control_rules([],OldNextS,OldNextP,OldNextS,OldNextP).
add_control_rules([_-Conseqs|T],OldNextS,OldNextP,NewNextS,NewNextP) :-
  (control_rule_incompatible(Conseqs,OldNextS) ->
    add_control_rules(T,OldNextS,OldNextP,NewNextS,NewNextP);
    add_rule_conseqs_to_next_states(Conseqs,OldNextS,OldNextP,[],[],IntNextS,
    IntNextP),
    add_control_rules(T,IntNextS,IntNextP,NewNextS,NewNextP)).

% get_control_consequences/5
% get_control_consequences(+ID,+Threshold,+PreTranState,-PostTranState,-Probs):
%   Given the Id of the action situation, the Threshold for the rules to be
%   considered and the PreTranState, return the set of PostTranStates and
%   respective Probs.
get_control_consequences(ID,Threshold,PreTranState,PostTranStates,Probs) :-
  find_consequences(ID,control,Threshold,L1),
  add_control_rules(L1,[[]],[1],L2,Probs),
  update_all_new_states(PreTranState,L2,PostTranStates).
