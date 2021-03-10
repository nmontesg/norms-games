#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  7 21:54:36 2021

@author: nmontes
"""

import streamlit as st

@st.cache(allow_output_mutation=True)
def cached_agents():
  return []

@st.cache(allow_output_mutation=True)
def cached_agent_attributes():
  return []

agents = cached_agents()
agent_attributes = cached_agent_attributes()

def app():
  st.markdown("""
              ## ``agents.pl``
              """)
              
  # --------------------------------------------------------------------------
  
  st.text("")
  
  # --------------------------------------------------------------------------
  
  # AGENTS
  
  agents_input = st.text_input(label="Introduce agent:",
                               value="alice",
                               key="agent-input")
  
  a1, a2 = st.beta_columns(2)
  add_agent_button = a1.button("Add", key="agent-add")
  if add_agent_button:
    if agents_input not in agents:
      agents.append(agents_input)
   
  reset_agents_button = a2.button("Clear all", key="agents-reset")
  if reset_agents_button:
    agents.clear()
      
  agents_dict = {(i+1):r for i,r in enumerate(agents)}
  
  a3, a4 = st.beta_columns(2)
  
  try:
    max_agents = max(agents_dict.keys())
  except ValueError:
    max_agents = 1
    
  select_remove_agent = a3.number_input("Select to remove:",
                                        min_value=1,
                                        max_value=max_agents,
                                        value=1,
                                        key="agents-remove")
  remove_agent_button = a4.button("Remove", key="agent-remove")
  if remove_agent_button:
    try:
      agent_to_be_removed = agents_dict[select_remove_agent]
      agents.remove(agent_to_be_removed)
      agents_dict = {(i+1):r for i,r in enumerate(agents)}
    except KeyError:
      pass
    
  st.markdown("**Your agents:**")
  st.write(agents_dict)
  
  # --------------------------------------------------------------------------
  
  for _ in range(3):
    st.text("")
    
  # --------------------------------------------------------------------------
  
  # AGENT ATTRIBUTES
  
  agent_attribute_input = st.text_input(label="Introduce agent attribute with\
                                        predicate of type \
                                        attributeName(+Agent, +Value):",
                                        key="agent-attributes-input")
  
  at1, at2= st.beta_columns(2)
  add_agent_attribute_button = at1.button("Add", key="agent-attribute-add")
  if add_agent_attribute_button:
    if agent_attribute_input not in agent_attributes:
      agent_attributes.append(agent_attribute_input)
      
  reset_agent_attributes_button = at2.button("Clear all",
                                            key="agent-attribute-reset")
  if reset_agent_attributes_button:
    agent_attributes.clear()
      
  agent_attributes_dict = {(i+1):r for i,r in enumerate(agent_attributes)}
  
  at3, at4 = st.beta_columns(2)
  try:
    max_agent_attributes = max(agent_attributes_dict.keys())
  except ValueError:
    max_agent_attributes = 1
    
  select_remove_agent_attribute = at3.number_input("Select to remove:",
                                                  min_value=1,
                                                  max_value=\
                                                    max_agent_attributes,
                                                  value=1,
                                                  key="agent-attr-remove")
  remove_agent_attribute_button = at4.button("Remove",
                                             key="agent-attr-remove")
  if remove_agent_attribute_button:
    try:
      attr_to_be_removed = \
        agent_attributes_dict[select_remove_agent_attribute]
      agent_attributes.remove(attr_to_be_removed)
      agent_attributes_dict = {(i+1):r for i,r in enumerate(agent_attributes)}
    except KeyError:
      pass
    
  st.markdown("**Your agent attributes:**")
  st.write(agent_attributes_dict)
