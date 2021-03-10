#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  7 22:10:40 2021

@author: nmontes
"""

import streamlit as st

@st.cache(allow_output_mutation=True)
def cached_rules():
  return []

boundary_rules = cached_rules()

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
              ### Boundary rules
              
              Boundary rules determine who, among the agents, gets to
              participate in the interaction.
              """)
              
  # --------------------------------------------------------------------------
  
  st.text("")
  
  # --------------------------------------------------------------------------          

  default_boundary_rule = st.text_area(
      label="By default, all agents participate:",
      value="""rule(
      <identifier>,
      boundary,
      0,
      if agent(A)
      then participates(A)
      where []
      ).""",
      height=225
    )
    
  use_default_boundary_rule = st.checkbox("Use default boundary rule")
  
  if use_default_boundary_rule:
    if default_boundary_rule not in boundary_rules:
      boundary_rules.append(default_boundary_rule)
  
  else:
    if default_boundary_rule in boundary_rules:
      boundary_rules.remove(default_boundary_rule)
    
  new_boundary_rule = st.text_area(
      label="Additional boundary rules:",
      value="""rule(
      <identifier>,
      boundary,
      <priority>,
      if agent(A) and ...
      then (~)participates(A)
      where [<constraints>]
      ).""",
      height=225
    )
  
  b1, b2 = st.beta_columns(2)
    
  add_boundary_rule_button = b1.button("Add rule", key="boundary-add")
  if add_boundary_rule_button:
    if new_boundary_rule not in boundary_rules:
      boundary_rules.append(new_boundary_rule)
      
  reset_boundary_rules_button = b2.button("Clear all", key="boundary-reset")
  if reset_boundary_rules_button:
    use_default_boundary_rule = False
    boundary_rules.clear()
      
  boundary_rules_dict = {(i+1):r for i,r in enumerate(boundary_rules)}
  
  c1, c2 = st.beta_columns(2)
  try:
    max_rules = max(boundary_rules_dict.keys())
  except ValueError:
    max_rules = 1
  select_remove_rule = c1.number_input("Select to remove:",
                                        min_value=1,
                                        max_value=max_rules,
                                        value=1,
                                        key="boundary-remove")
  remove_rule_button = c2.button("Remove", key="boundary-remove")
  if remove_rule_button:
    try:
      to_be_removed = boundary_rules_dict[select_remove_rule]
      boundary_rules.remove(to_be_removed)
      boundary_rules_dict = {(i+1):r for i,r in enumerate(boundary_rules)}
    except KeyError:
      pass
  
  st.markdown("**Your boundary rules:**")
  st.write(boundary_rules_dict)
  