#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  7 22:13:18 2021

@author: nmontes
"""

import streamlit as st

@st.cache(allow_output_mutation=True)
def cached_rules():
  return []

choice_rules = cached_rules()

def app():
  st.markdown("""
              ## ``rules.pl``
              
              In this file we rules and norms that structure the situation we
              intend to model (relevant state variables, available actions and
              their effects, etc).
              
              The rules follow the syntax:
              
                  rule(
                      flag,
                      type,
                      priority,
                      if Conditions then Consequences where Constraints
                  ).
              
              
              * ``flag``: an identifier for the situation we are modelling.
              
              * ``type``: the aspect of the action situation that the rule is
              targeting.
              
              * ``priority``: 0 for the *default* rules, >0 for norms
              overruling or adding on top of the default rules.
              
              * ``Conditions-Consequences-Constraints``: the actual content of
              the rule.
              """)
              
  # --------------------------------------------------------------------------
  
  for _ in range(3):
    st.text("")
  
  # --------------------------------------------------------------------------
              
  st.markdown("""
              ### Choice rules
              
              Choice rules determine what actions ara available to every role, 
              and under what circumstances.
              """)
              
  # --------------------------------------------------------------------------
  
  st.text("")
  
  # --------------------------------------------------------------------------
  
  new_choice_rule = st.text_area(
      label="Add choice rules:",
      value="""rule(
      <identifier>,
      choice,
      <priority>,
      if role(A,<role>)
      then (~)can(A,<action>)
      where [<constraints>]
      ).""",
      height=225
    )
    
  ch1, ch2 = st.beta_columns(2)
    
  add_choice_rule_button = ch1.button("Add rule", key="choice-add")
  if add_choice_rule_button:
    if new_choice_rule not in choice_rules:
      choice_rules.append(new_choice_rule)
      
  reset_choice_rules_button = ch2.button("Clear all", key="choice-reset")
  if reset_choice_rules_button:
    choice_rules.clear()
      
  choice_rules_dict = {(i+1):r for i,r in enumerate(choice_rules)}
  
  c1, c2 = st.beta_columns(2)
  try:
    max_rules = max(choice_rules_dict.keys())
  except ValueError:
    max_rules = 1
  select_remove_rule = c1.number_input("Select to remove:",
                                        min_value=1,
                                        max_value=max_rules,
                                        value=1,
                                        key="choice-remove")
  remove_rule_button = c2.button("Remove", key="choice-remove")
  if remove_rule_button:
    try:
      to_be_removed = choice_rules_dict[select_remove_rule]
      choice_rules.remove(to_be_removed)
      choice_rules_dict = {(i+1):r for i,r in enumerate(choice_rules)}
    except KeyError:
      pass
    
  st.markdown("**Your choice rules:**")
  st.write(choice_rules_dict)
  