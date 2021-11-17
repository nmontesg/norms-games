/** writer.pl Action situation description generator

Given a default rule configuration, a norm repository and a normative system,
update the action situation description files so that they van be processed
with ngames/evaluation/build.build_full_game, to build the game representation.
The normative system in question consists of a mapping of a subset of the norms
in the repository to an strictly positive integer. All norms in the
normative system must map to a distinct integer.

@author Nieves Montes
@email nmontes@iiia.csic.es
@copyright (c)2021 Nieves Montes.
@license MIT
*/

:- ensure_loaded(ngames/evaluation/interpreter).

%! copy_default(+DefaultsPath:str) is det
%
% Copy the action situation description files containing the default
% configuration into the current directory. The copied files are modified, not
% the original ones.
%
% @param DefaultsPath Path to folder with default action situation description.
copy_default(DefaultsPath) :-
    string_concat(DefaultsPath,'/states.pl',StatesFile),
    string_concat(DefaultsPath,'/agents.pl',AgentsFile),
    string_concat(DefaultsPath,'/rules.pl',RulesFile),
    copy_default_file(StatesFile,'states.pl'),
    copy_default_file(AgentsFile,'agents.pl'),
    copy_default_file(RulesFile,'rules.pl').

%! copy_default_file(+FromFile:str,+ToFile:str) is det
%
% Auxiliary predicate to copy a single file.
%
% @param FromFile Path to file to be copied.
% @param ToFile Path where the copy is to be stored.
copy_default_file(FromFile,ToFile) :-
    open(FromFile,read,InStream),
    open(ToFile,write,OutStream),
    copy_stream_data(InStream,OutStream),
    close(InStream),
    close(OutStream).

%! update_action_situation_description(+NormRepository:str,+Defaults:str,+NormativeSystem:dict,+ID:str) is det
%
% Update a default action situation description with the additional norms
% (and their respective priorities) of a normative system.
%
% @param NormRepository Path to the repository of additional norms.
% @param Default Path to the files that contain the default description.
% @param NormativeSystem Dictionary of name keys and strictly positive integer
%                        values, that correspond to the priorities that their
%                        rules will be assigned.
% @param ID Action situation identifier.
update_action_situation_description(NormRepository,
        NormativeSystem,ID) :-
    open(NormRepository,read,InStream),
    process_norm_stream(InStream,NormativeSystem,ID),
    close(InStream).

%! process_norm_stream(+InStream:stream,+NormativeSystem:dict,+ID:str) is det
%
% Process the repository of additional norms and add the corresponding rules
% and other predicates for the norms that are included in the normative system.
%
% @param InStream Norm repository input stream.
% @param NormativeSystem Dictionary of name keys and strictly positive integer
%                        values, that correspond to the priorities that their
%                        rules will be assigned.
% @param ID Action situation identifier.
process_norm_stream(InStream,NormativeSystem,ID) :-
    read(InStream,Term),
    Term\=end_of_file,!,
    dict_create(Norm,norm,Term),
    Name=Norm.name,
    (Priority=NormativeSystem.get(Name) ->
        process_norm_dict(Norm,Priority,ID);
    true),
    process_norm_stream(InStream,NormativeSystem,ID).
process_norm_stream(_,_,_).

%! process_norm_dict(+Norm:dict,+Priority:int,ID:str) is det
%
% Add a norm to the action situation description file. Its 'rule/4`
% statements are assigned priotity given by 'Priority`.
%
% @param Norm A dictionary with all the information related to the norm.
%             Retrieved from the norm repository.
% @param Priority Strictly positive integer.
% @param ID Action situation identifier.
process_norm_dict(Norm,Priority,ID) :-
    % STEP 1: ADDITIONS TO states.pl
    open('states.pl',append,StatesStream),
    write(StatesStream,"\n%----------------------------------------------\n"),
    PredicateList=Norm.predicates,
    IncompatiblesList=Norm.incompatibles,
    InitialList=Norm.initially,
    add_predicates(PredicateList,StatesStream),
    add_incompatibles(IncompatiblesList,StatesStream),
    add_initials(InitialList,StatesStream),
    close(StatesStream),
    % STEP 2: ADDITIONS TO rules.pl
    open('rules.pl',append,RulesStream),
    write(RulesStream,"\n%----------------------------------------------\n"),
    AllRulesList=Norm.rules,
    dict_create(AllRulesDict,rules,AllRulesList),
    % write(AllRulesDict),nl,
    BoundaryRules=AllRulesDict.boundary,
    PositionRules=AllRulesDict.position,
    ChoiceRules=AllRulesDict.choice,
    ControlRules=AllRulesDict.control,
    add_rules(BoundaryRules,ID,boundary,Priority,RulesStream),
    add_rules(PositionRules,ID,position,Priority,RulesStream),
    add_rules(ChoiceRules,ID,choice,Priority,RulesStream),
    add_rules(ControlRules,ID,control,Priority,RulesStream),
    close(RulesStream).

%! add_predicates(+L:list,+Stream:stream) is det
%
% Add the predicates as dynamic to the corresponding file of the action
% situation description.
%
% @param L List of predicate/arity to be included as dynamic.
% @param Stream Output file stream.
add_predicates([],_).
add_predicates([H|T],Stream) :-
    write(Stream,:-dynamic H),
    write(Stream,".\n"),
    add_predicates(T,Stream).

%! add_incompatibles(+L:list,+Stream:stream) is det
%
% Add the predicates as incompatible to the corresponding file of the action
% situation description.
%
% @param L List of predicates to be included as incompatible.
% @param Stream Output file stream.
add_incompatibles([],_).
add_incompatibles([H|T],Stream) :-
    write(Stream,incompatible(H,L) :- member(H,L)),
    write(Stream,".\n"),
    add_incompatibles(T,Stream).

%! add_initials(+L:list,+Stream:stream) is det
%
% Add the predicates as initial conditions to the corresponding file of the
% action situation description.
%
% @param L List of predicates to be included as initial conditions.
% @param Stream Output file stream.
add_initials([],_).
add_initials([H|T],Stream) :-
    write(Stream,H),
    write(Stream,".\n"),
    add_initials(T,Stream).

%! add_rules(+L:list,+ID:str,+Type:str,+Priority:int,+Stream:stream) is det
%
% Add the rules to the action situation description, with the input
% priority and rule type.
%
% @param L List of rule content arguments to be added.
% @param ID Action situation identifier.
% @param Type Rule type. One of either 'boundary`, 'position`, 'choice`,
%             'control` or 'payoff`.
% @param Priority Strictly positive integer.
% @param Stream Output file stream.
add_rules([],_,_,_,_).
add_rules([H|T],ID,Type,Priority,Stream) :-
    Rule=rule(ID,Type,Priority,H),
    write(Stream,Rule),
    write(Stream,".\n"),
    add_rules(T,ID,Type,Priority,Stream).

%! build_ASL_description(+DefaultsPath:str,+NormsPath:str,+NormativeSystem:dict,+ID:str) is det
%
% Make a copy of the default action situation description files and expand it
% with the norms selected from a norm repository by the input normative system.
%
% @param DefaultsPath Path to the folder with the default action situation
%                     description files.
% @param NormsPath Path to the norm repository file.
% @param NormativeSystem Dictionary mapping norms in the repository to a
%                        strictly positive integer (aka the priority that
%                        the norm has).
% @param ID Action situation identifier.
build_ASL_description(DefaultsPath,NormsPath,NormativeSystem,ID) :-
    copy_default(DefaultsPath),
    update_action_situation_description(NormsPath,
        NormativeSystem,ID).
