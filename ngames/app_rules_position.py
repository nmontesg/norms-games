#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  7 22:11:28 2021

@author: nmontes
"""

import streamlit as st

@st.cache(allow_output_mutation=True)
def cached_rules():
  return []

position_rules = cached_rules()

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
              ### Position rules
              
              Position rules determine what role does each partcipant take.
              """)
              
  # --------------------------------------------------------------------------
  
  st.text("")
  
  # --------------------------------------------------------------------------
  
  new_position_rule = st.text_area(
      label="Add position rules:",
      value="""rule(
      <identifier>,
      position,
      <priority>,
      if participates(A) and ...
      then role(A,<role>)
      where [<constraints>]
      ).""",
      height=225
    )
    
  p1, p2 = st.beta_columns(2)
    
  add_position_rule_button = p1.button("Add rule", key="position-add")
  if add_position_rule_button:
    if new_position_rule not in position_rules:
      position_rules.append(new_position_rule)
      
  reset_position_rules_button = p2.button("Clear all", key="position-reset")
  if reset_position_rules_button:
    position_rules.clear()
      
  position_rules_dict = {(i+1):r for i,r in enumerate(position_rules)}
  
  c1, c2 = st.beta_columns(2)
  try:
    max_rules = max(position_rules_dict.keys())
  except ValueError:
    max_rules = 1
  select_remove_rule = c1.number_input("Select to remove:",
                                        min_value=1,
                                        max_value=max_rules,
                                        value=1,
                                        key="position-remove")
  remove_rule_button = c2.button("Remove", key="position-remove")
  if remove_rule_button:
    try:
      to_be_removed = position_rules_dict[select_remove_rule]
      position_rules.remove(to_be_removed)
      position_rules_dict = {(i+1):r for i,r in enumerate(position_rules)}
    except KeyError:
      pass
  
  st.markdown("**Your position rules:**")
  st.write(position_rules_dict)
  