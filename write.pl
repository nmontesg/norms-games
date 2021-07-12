write_rule_from_DB(Rule_DB_file, Rule_output_file, Priority_value_pairs) :-
  open(Rule_DB_file,read,In),
  open(Rule_output_file,write,Out),
  read_rule(In,Out,Priority_value_pairs),
  close(In),
  close(Out).


read_rule(Input_stream,_,_) :-
  read(Input_stream,end_of_file),!.

read_rule(Input_stream,Output_stream,Priority_value_pairs) :-
  read(Input_stream,Rule),
  Rule = rule(ID,Type,Priority, if Condition then Consequence where Constraints),
  (
  Priority is 0 ->
  write(Output_stream,Rule),
  write(Output_stream,.),
  write(Output_stream,'\n\n')
  ;
    (
    member(Priority-N, Priority_value_pairs) ->
    write(Output_stream,rule(ID,Type,N, if Condition then Consequence where Constraints)),
    write(Output_stream,.),
    write(Output_stream,'\n\n')
    ;
    true
    )
  ),
  read_rule(Input_stream,Output_stream,Priority_value_pairs).
