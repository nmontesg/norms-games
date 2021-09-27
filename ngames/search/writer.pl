:- ensure_loaded(ngames/evaluation/interpreter).

% TODO: documentation in PlDoc format

copy_default(DefaultsPath) :-
    string_concat(DefaultsPath,'/states.pl',StatesFile),
    string_concat(DefaultsPath,'/agents.pl',AgentsFile),
    string_concat(DefaultsPath,'/rules.pl',RulesFile),
    copy_default_file(StatesFile,'states.pl'),
    copy_default_file(AgentsFile,'agents.pl'),
    copy_default_file(RulesFile,'rules.pl').

copy_default_file(FromFile,ToFile) :-
    open(FromFile,read,InStream),
    open(ToFile,write,OutStream),
    copy_stream_data(InStream,OutStream),
    close(InStream),
    close(OutStream).

% update_action_situation_description/4
% update_action_situation_description(+NormRepository,+Defaults,+NormativeSystem,
%   +ID):
%   NormRepository: Path to the repository of additional norms
%   Default: Path to the files that contain the default description
%   NormativeSystem: Dictionary of name keys and integer values (>=1)
%   ID: Action Situation identifier
update_action_situation_description(NormRepository,
        NormativeSystem,ID) :-
    open(NormRepository,read,InStream),
    process_norm_stream(InStream,NormativeSystem,ID),
    close(InStream).

process_norm_stream(InStream,_,_) :-
    read(InStream,end_of_file),!.
process_norm_stream(InStream,NormativeSystem,ID) :-
    read(InStream,Term),
    Term\=end_of_file,
    dict_create(Norm,norm,Term),
    Name=Norm.name,
    (Priority=NormativeSystem.get(Name) ->
        process_norm_dict(Norm,Priority,ID);
    true),
    process_norm_stream(InStream,NormativeSystem,ID).

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

add_predicates([],_).
add_predicates([H|T],Stream) :-
    write(Stream,:-dynamic H),
    write(Stream,".\n"),
    add_predicates(T,Stream).

add_incompatibles([],_).
add_incompatibles([H|T],Stream) :-
    write(Stream,incompatible(H,L) :- member(H,L)),
    write(Stream,".\n"),
    add_incompatibles(T,Stream).

add_initials([],_).
add_initials([H|T],Stream) :-
    write(Stream,H),
    write(Stream,".\n"),
    add_initials(T,Stream).

add_rules([],_,_,_,_).
add_rules([H|T],ID,Type,Priority,Stream) :-
    Rule=rule(ID,Type,Priority,H),
    write(Stream,Rule),
    write(Stream,".\n"),
    add_rules(T,ID,Type,Priority,Stream).

build_ASL_description(DefaultsPath,NormsPath,NormativeSystem,ID) :-
    copy_default(DefaultsPath),
    update_action_situation_description(NormsPath,
        NormativeSystem,ID).
