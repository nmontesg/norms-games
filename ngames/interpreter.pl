/** <module> Action Situation Language interpreter

This module includes the definitions of common operators and predicates.
It also includes the predicates to query the rule base according to the
established facts in the knowledge base and process their consequences
into a Python-friendly format (mostly as lists).

@author Nieves Montes
@email nmontes@iiia.csic.es
@copyright (c)2021 Nieves Montes.
@license MIT
*/

:- use_module(library(clpr)).

% predicates for agents, participants, roles, actions and payoffs
:- dynamic agent/1, role/2, participates/1, can/2, does/2.
:- discontiguous (agent)/1, (role)/2, (participates)/1, (can)/2, (does)/2.

% predicates for initial conditions and compatibility
:- dynamic initially/1, terminal/0, incompatible/2.
:- discontiguous (initially)/1, (terminal)/0, (incompatible)/2.

% operators for if-then rules
:- dynamic rule/4.
:- discontiguous (rule)/4.

:- op(600, fx, if).
:- op(500, xfx, then).
:- op(400, xfx, where).
:- discontiguous (if)/1, (then)/2, (where)/2.

:- op(200, xfy, and).
:- op(150, fx, ~).
:- dynamic and/2, (~)/1.
:- discontiguous (and)/2, (~)/1.

:- op(350, yfx, withProb).
:- discontiguous (withProb)/2.

% # General-purpose predicates

% To query all rules and in particular to process the consequences of simple
% rules (boundary, position, choice and payoff).

%! query(?Term) is det
%
% Substitute for the usual call/1 predicate, but includes support for
% terms expressed as conjunctions (e.g. A and B).
%
% @param Term A Prolog term to be queried.

query(A) :-
    call(A).

query(A and B) :-
    query(A),
    query(B).


%! query_rule(?Rule) is det.
%
% Return True if Rule is active given the current state of the system.
%
% @param Rule A _rule/4_ statement.

query_rule(rule(ID,Type,Priority,if Condition then Consequence where Constraints)) :-
	rule(ID,Type,Priority,if Condition then Consequence where Constraints),
	maplist(query,[Condition|Constraints]).


%! find_consequences(+ID,+Type,+Threshold,-L) is det
%
% Find the instantitations of the if-then-where rules of the _Type_ kind
% that are currently active, and extract their consequences paired with their
% priority. Active rules with priority larger than _Threshold_ are excluded.
%
% @param ID Identifier for the action situation.
% @param Type One of either _boundary_, _position_, _choice_ or _control_.
% @param Threshold The maximum priority of rules to be considered.
% @param L The output list with the processed consequences.

find_consequences(ID,Type,Threshold,L) :-
    Rule = rule(ID,Type,Priority,if _ then Consequence where _),
    findall(Priority-Consequence,query_rule(Rule),L1),
    delete_key_gt(L1,Threshold,L2),
    keysort(L2,L3),
    reverse(L3,L).


%! delete_key_gt(L,N,NewL) is det
%
% Auxiliary predicate to delete consequences whose priority is over some
% given threshold.
%
% @param L List with priority-fluents pairs to be processed.
% @param N Integer threshold, fluents with priority over it are to be excluded.
% @param NewL List identical to _L_, but fluents with priority over Threshold
% are excluded.

delete_key_gt([],_,[]) :- !.
delete_key_gt([H|T],N,L) :-
    H = Priority-_,
    Priority>N,!,
    delete_key_gt(T,N,L).
delete_key_gt([H|T],N,[H|L]) :-
    delete_key_gt(T,N,L).


%! process_consequences(+Conseqs,+OldParts,?NewParts) is det
%
% Get all the consequences of some rule type and process them in decreasing
% order of priority. It returns a new list indicating the consequences that
% hold, negated ones (aka overwritten) included. The list of consequences
% is processed in a way such that consequences that are in conflict with
% consequences of higher priority are discarded.
%
% It should be called as:
%
% ?- find_consequences(boundary,L),process_consequences(L,[],P).
%
% @param Conseqs List of consequence of some rule type.
% @param OldParts List of old consequences. Intended to be called with the
% empty list.
% @param NewParts List of the consequences that hold, negations included.

process_consequences([],OldPartList,OldPartList).
process_consequences(ConseqList,OldPartList,NewPartList) :-
    ConseqList = [_-V1|T],
    add_conseq(V1,OldPartList,IntPartList),
    process_consequences(T,IntPartList,NewPartList).

add_conseq(C,Old,Old) :- member(C,Old),!.
add_conseq(C,Old,Old) :- member(~C,Old),!.
add_conseq(C,Old,[C|Old]).


%! get_simple_consequences(+ID,+Type,+Threshold,-L) is det
%
% Process the consequences of boundary, position, choice and payoff rules.
% It gets the consequences of the rules with the given identification and
% type, has rules of higher priority overwrite rules of lower priority, and
% finally deletes negated (aka overwritten) facts. It returns the result in
% a list of fluents.
%
% @param ID Identifier of the action situation.
% @param Type One of either _boundary_, _position_, _choice_ or _payoff_.
% @param Threshold Consequences of rules with priorities exceeding _Threshold_
% are excluded.

get_simple_consequences(ID,Type,Threshold,L) :-
    find_consequences(ID,Type,Threshold,L1),
    process_consequences(L1,[],L2),
    delete(L2,~_,L).


%%-----------------------------------------------------------------------------

% # Processing control rules

% Predicates to find whether a triggered control rule is compatible
% with the potential next states already established.

%! control_conseq_fact_incompatible(+Fact,+S) is det
%
% Check whether a single fact is compatible with a list of established facts.
%
% @param Fact The fluent whose compatibility we want to check.
% @param S List of previously established facts.

control_conseq_fact_incompatible(_,[]) :- !.
control_conseq_fact_incompatible(F,[S1|S2]) :-
    incompatible(F,S1),
    control_conseq_fact_incompatible(F,S2).


%! control_conseq_incompatible(+Facts,+S) is det
%
% Checks whether the fluents in _Facts_ that make up a joint consequence
% statement of an active control rule are incompatible with the next states
% already derived in _S_.
%
% @param Facts Facts derived from a new control rule. Either a single fact
% or a conjunction of them (aka _A and B_).
% @param S List of potential next state descriptions already derived
% (aka a list of lists).

control_conseq_incompatible(F1 and F2,S) :-
    control_conseq_fact_incompatible(F1,S),
    control_conseq_incompatible(F2,S).
control_conseq_incompatible(F,S) :-
    control_conseq_fact_incompatible(F,S).


%! control_rule_incompatible(+Conseqs,+S) is det
%
% Check whether the _Conseqs_ list of an active control rule is compatible
% with the next states already derived in _S_.
%
% @param Conseqs List of facts derived from a control rule. Each of them is
% either a single fact or a conjunction (aka _A and B_).
% @param S List of potential next state descriptions already derived
% (aka a list of lists).

control_rule_incompatible([],_).
control_rule_incompatible([C withProb _|T],S) :-
    control_conseq_incompatible(C,S),
    control_rule_incompatible(T,S).


% Predicates to add the consequences of a single rule into the set of
% potential next states.

%! add_rule_conseqs_to_next_states(+Conseqs,+S,+P,+OldNextS,+OldNextP,-NewNextS,-NewNextP) is det
%
% Given the potential next states derived in _S_ and their probabilities
% in _P_, add the consequences in _Conseqs_ of an active control rule.
%
% Intended to be called as:
%
% ?- add_rule_conseqs_to_next_states(Conseqs,S,P,[],[],NewS,NewP).
%
% @param Conseqs A list of consequences from an active control rule,
% with the format _Facts withProb P_.
% @param S List of alredy derived next states.
% @param P List of the probabilities of the already derived next states.
% @param OldNextStates List of partially processed next states.
% @param OldNextP List of partially processed next states probabilities.
% @param NewNextS List of next states after processing all of _Conseqs_.
% @param NewNextP List of next states probabilities after processing all
% of _Conseqs_.

add_rule_conseqs_to_next_states([],_,_,OldNextStates,OldNextP,OldNextStates,
        OldNextP).
add_rule_conseqs_to_next_states([C withProb X|T],S,P,OldNextStates,OldNextP,
        NewNextStates,NewNextP) :-
    add_joint_conseqs_to_next_states(C withProb X,S,P,NewS,NewP),
    append(NewS,OldNextStates,IntNextStates),
    append(NewP,OldNextP,IntNextP),
    add_rule_conseqs_to_next_states(T,S,P,IntNextStates,IntNextP,NewNextStates,
    NewNextP).


%! add_joint_conseqs_to_next_states(+F,+OldNextStates,+OldProb,-NewNextStates,-NewProb) is det
%
% Given a fact (or conjunction of facts) alonside with their probability
% (_C withProb P_), derived from an activated control rule, update the list of
% _OldNextStates_ and their probabilities _OldProb_ into _NewNextStates_
% and _NewProb_.
%
% @param F Consequence derived from an active control rule, joint by
% operator _withProb_ to its probability.
% @param OldNextStates List of next states prior to update.
% @param OldProb List of next states probabilities prior to update.
% @param NewNextStates List of updated next states.
% @param NewProb List of updated next states probabilities.

add_joint_conseqs_to_next_states(_,[],[],[],[]).
add_joint_conseqs_to_next_states(C withProb X,[OldS1|OldS2],[OldP1|OldP2],
        [NewS1|NewS2],[NewP1|NewP2]) :-
    add_joint_conseqs_to_single_state(C withProb X,OldS1,OldP1,NewS1,NewP1),
    add_joint_conseqs_to_next_states(C withProb X,OldS2,OldP2,NewS2,NewP2).


%! add_joint_conseqs_to_single_state(+F,+State,+Prob,-NewState,-NewProb) is det
%
% Takes one consequence fact _F_ (expressed as _C withProb P_)
% from an activated control rule, a state _S_ to be updated (as a list of
% fluents) with probability _Prob_ and return the updated state fluents
% and probability.
%
% @param F A fact (or conjunction of facts) to append to the partially
% derived state _S_.
% @param State List of facts that make up a partially derived state.
% @param Prob Intermediate probability of the partially derived state.
% @param NewState List of updated state fluents.
% @param NewProb Updated probability (i.e. product of intermediate state
% probability and probability of the derived facts).

add_joint_conseqs_to_single_state(C withProb X,State,Prob,NewState,NewProb) :-
    joint_conseqs_to_list([],C,L),
    append(State,L,NewState),
    NewProb is X*Prob.


%! joint_conseqs_to_list(+Old,+F,-New) is det
%
% Auxiliary predicate to append a fact or a conjunction of facts
% (aka _A and B_) to a list of fluent.
%
% @param Old List where new fluents are to be appended.
% @param F Fact or conjunction of facts to be appended.
% @param New Updated list of facts.

joint_conseqs_to_list(Old,C1 and C2,New) :-
    !,joint_conseqs_to_list([C1|Old],C2,New).
joint_conseqs_to_list(Old,C1,[C1|Old]).


% Predicates to drag the compatible facts from the pre-transition state
% to the potential next states.

%! drag_compatible_fact(+PreTranFact,+PostTranState,-UpdatedPostTransState) is det
%
% If compatible, update a provisional next state with a fluent from the pre-
% transition state.
%
% @param PreTranFact A fluent that holds true in some pre-transition state.
% @param PostTranState Partially constructed post-transition state.
% @param UpdatedPostTransState Post transition state updated with _PreTranFact_.

drag_compatible_fact(PreTranFact,NewState,NewState) :-
    incompatible(PreTranFact,NewState),!.
drag_compatible_fact(PreTranFact,NewState,[PreTranFact|NewState]).


%! drag_compatible_fact(+PreTranFact,+PostTranState,-UpdatedPostTransState) is det
%
% Update a partially constructed post-transition state _PostTranState_ with
% the compatible facts from pre-transition state _PreTranFact_
%
% @param PreTranFact List of pretransition state facts.
% @param PostTranState Partially constructed post-transition state, as a list
% of facts.
% @param UpdatedPostTransState Updated post-transition state.

drag_compatible_state([],NewState,NewState).
drag_compatible_state([F|T],NewState,UpdatedNewState) :-
    drag_compatible_fact(F,NewState,Int),
    drag_compatible_state(T,Int,UpdatedNewState).


%! update_all_new_states(+PreTranState,+PostTranStates,-UpdatedPostTransStates) is det
%
% Update all the potential next states in _PostTranStates_ with the compatible
% facts in _PreTranState_.
%
% @param PreTranState List of fluents in the pre-transition state.
% @param PostTranStates Partially constructed post-transition states 
% (aka a list of lists).
% @param UpdatedPostTransStates Updated post-transition states.

update_all_new_states(_,[],[]).
update_all_new_states(PreTranState,[S1|S2],[NewS1|NewS2]) :-
    drag_compatible_state(PreTranState,S1,NewS1),
    update_all_new_states(PreTranState,S2,NewS2).


% Predicates to find the next state based on the actions performed.

%! add_control_rules(+RuleConseqs,+OldNextS,+OldNextP,-NewNextS,-NewNextP) is det
%
% Given the priority-consequences pairs of the activated control rules in
% _RuleConseqs_, append them to the provisional states in _OldNextS_
% with their unadapted probabilities in _OldNextP_. If the rule is compatible
% with the facts already established, add the consequences into _NewNextS_
% and update the probabilities into _NewNextP_.
%
% @param RuleConseqs List of priority-facts consequences derived from the
% activated control rules.
% @param OldNextS List of unupdated post-transition states (aka a list of
% lists).
% @param OldNextP List of unupdated probabilities for the post-transition
% states.
% @param NewNextS List of updated post-transition states (aka a list of lists).
% @param NewNextP List of updated post-transition states probabilities.

add_control_rules([],OldNextS,OldNextP,OldNextS,OldNextP).
add_control_rules([_-Conseqs|T],OldNextS,OldNextP,NewNextS,NewNextP) :-
    (control_rule_incompatible(Conseqs,OldNextS) ->
        add_control_rules(T,OldNextS,OldNextP,NewNextS,NewNextP);
        add_rule_conseqs_to_next_states(Conseqs,OldNextS,OldNextP,[],[],
                                        IntNextS,IntNextP),
        add_control_rules(T,IntNextS,IntNextP,NewNextS,NewNextP)).


%! get_control_consequences(+ID,+Threshold,+PreTranState,-PostTranState,-Probs) is det
%
% Gather the consequences of control rules given the current state for the
% action situation of interest. The rules whose priority exceeds _Threshold_
% are excluded.
%
% @param ID Identifier of the action situation.
% @param Threshold Rules whose priority exceed it are excluded from processing.
% @param PreTranState List of pre-transition state facts.
% @param PostTranState List of possible post-transition states
% (aka a list of lists).
% @param Probs List of probabilities of the post-transition states.

get_control_consequences(ID,Threshold,PreTranState,PostTranStates,Probs) :-
    find_consequences(ID,control,Threshold,L1),
    add_control_rules(L1,[[]],[1],L2,Probs),
    update_all_new_states(PreTranState,L2,PostTranStates).
