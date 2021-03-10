#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  7 22:13:56 2021

@author: nmontes
"""

import streamlit as st
from app_rules_boundary import rule_header

@st.cache(allow_output_mutation=True)
def cached_rules():
  return []

control_rules = cached_rules()

def app():
  st.markdown(rule_header)
              
  # --------------------------------------------------------------------------
  
  for _ in range(3):
    st.text("")
  
  # --------------------------------------------------------------------------
              
  st.markdown("""
              ### Control rules
              
              Control rules determine the effects, together with their 
              probability, that actions have.
              """)
              
  # --------------------------------------------------------------------------
  
  st.text("")
  
  # --------------------------------------------------------------------------
  
  new_control_rule = st.text_area(
      label="Add control rules:",
      value="""rule(
      <identifier>,
      control,
      <priority>,
      if does(A,<action>) and ...
      then [<consequence1> withProb p, ...]
      where [<constraints>]
      ).""",
      height=225
    )
    
  co1, co2 = st.beta_columns(2)
    
  add_control_rule_button = co1.button("Add rule", key="control-add")
  if add_control_rule_button:
    if new_control_rule not in control_rules:
      control_rules.append(new_control_rule)
      
  reset_control_rules_button = co2.button("Clear all", key="control-reset")
  if reset_control_rules_button:
    control_rules.clear()
      
  control_rules_dict = {(i+1):r for i,r in enumerate(control_rules)}
  
  c1, c2 = st.beta_columns(2)
  try:
    max_rules = max(control_rules_dict.keys())
  except ValueError:
    max_rules = 1
  select_remove_rule = c1.number_input("Select to remove:",
                                        min_value=1,
                                        max_value=max_rules,
                                        value=1,
                                        key="control-remove")
  remove_rule_button = c2.button("Remove", key="control-remove")
  if remove_rule_button:
    try:
      to_be_removed = control_rules_dict[select_remove_rule]
      control_rules.remove(to_be_removed)
      control_rules_dict = {(i+1):r for i,r in enumerate(control_rules)}
    except KeyError:
      pass
    
  st.markdown("**Your control rules:**")
  st.write(control_rules_dict)