#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  7 21:56:09 2021

@author: nmontes
"""

import streamlit as st

@st.cache(allow_output_mutation=True)
def cached_dynamic_predicates():
  return []

@st.cache(allow_output_mutation=True)
def cached_biophysical_features():
  return []

@st.cache(allow_output_mutation=True)
def cached_initial_conditions():
  return []

@st.cache(allow_output_mutation=True)
def cached_terminal_conditions():
  return []

@st.cache(allow_output_mutation=True)
def cached_compatibility_predicates():
  return []

dynamic_predicates = cached_dynamic_predicates()
biophysical_features = cached_biophysical_features()
initial_conditions = cached_initial_conditions()
terminal_conditions = cached_terminal_conditions()
compatibility_predicates = cached_compatibility_predicates()

def app():
  st.markdown("""
              ## ``states.pl``
              """)

  # --------------------------------------------------------------------------

  st.text("")

  # --------------------------------------------------------------------------

  # DYNAMIC STATE PREDICATES

  dynamic_predicates_input = st.text_input(label="Introduce all the \
                                           predicate/arity pairs that \
                                          determine every state:")
  dy1, dy2 = st.beta_columns(2)
  add_dynamic_predicate_button = dy1.button("Add", key="dynamic-add")
  if add_dynamic_predicate_button:
    if dynamic_predicates_input not in dynamic_predicates:
      dynamic_predicates.append(dynamic_predicates_input)
  reset_dynamic_predicates = dy2.button("Clear all", key="dynamic-reset")
  if reset_dynamic_predicates:
    dynamic_predicates.clear()

  dynamic_predicates_dict = {(i+1):ini for i,ini in
                             enumerate(dynamic_predicates)}

  dy1, dy2 = st.beta_columns(2)
  try:
    max_pred = max(dynamic_predicates_dict.keys())
  except ValueError:
    max_pred = 1
  select_remove_dynamic_predicates = dy1.number_input("Select to remove:",
                                                     min_value=1,
                                                     max_value=max_pred,
                                                     value=1,
                                                     key="dynamic-remove")
  remove_dynamic_predicates_button = dy2.button("Remove", key="dynamic-remove")
  if remove_dynamic_predicates_button:
    try:
      to_be_removed = \
        dynamic_predicates_dict[select_remove_dynamic_predicates]
      dynamic_predicates.remove(to_be_removed)
      dynamic_predicates_dict = {(i+1):r for i,r in
                                 enumerate(dynamic_predicates)}
    except KeyError:
      pass

  st.markdown("**Your state predicates:**")
  st.write(dynamic_predicates_dict)

  # --------------------------------------------------------------------------

  for _ in range(3):
    st.text("")

  # --------------------------------------------------------------------------

  # BIOPHYSICAL FEATURES

  biophysical_features_input = st.text_input(label="Introduce the relevant \
                                             biophysical features of the \
                                            environment where the \
                                              interaction takes place.")
  bi1, bi2 = st.beta_columns(2)
  add_biophysical_features_button = bi1.button("Add", key="biophysical-add")
  if add_biophysical_features_button:
    if biophysical_features_input not in biophysical_features:
      biophysical_features.append(biophysical_features_input)
  reset_biophysical_features = bi2.button("Clear all", key="biophysical-reset")
  if reset_biophysical_features:
    biophysical_features.clear()

  biophysical_features_dict = {(i+1):ini for i,ini in
                               enumerate(biophysical_features)}

  bi3, bi4 = st.beta_columns(2)
  try:
    max_bio = max(biophysical_features_dict.keys())
  except ValueError:
    max_bio = 1
  select_remove_biophysical_features = bi3.number_input("Select to remove:",
                                                     min_value=1,
                                                     max_value=max_bio,
                                                     value=1,
                                                     key="biophysical-remove")
  remove_biophysical_features_button = bi4.button("Remove",
                                                  key="biophysical-remove")
  if remove_biophysical_features_button:
    try:
      to_be_removed = \
        biophysical_features_dict[select_remove_biophysical_features]
      biophysical_features.remove(to_be_removed)
      biophysical_features_dict = {(i+1):r for i,r in
                                 enumerate(biophysical_features)}
    except KeyError:
      pass

  st.markdown("**Your biophysical features:**")
  st.write(biophysical_features_dict)

  # --------------------------------------------------------------------------

  for _ in range(3):
    st.text("")

  # --------------------------------------------------------------------------

  # INITIAL CONDITIONS

  new_initial_condition = st.text_input("Add initial condition using the \
                                        predicate initially/1:",
                                        value="initially(...)")
  i1, i2 = st.beta_columns(2)
  add_initial_condition_button = i1.button("Add", key="initial-add")
  if add_initial_condition_button:
    if new_initial_condition not in initial_conditions:
      initial_conditions.append(new_initial_condition)
  reset_initial_conditions = i2.button("Clear all", key="initial-reset")
  if reset_initial_conditions:
    initial_conditions.clear()

  initial_conditions_dict = {(i+1):ini for i,ini in
                             enumerate(initial_conditions)}

  i3, i4 = st.beta_columns(2)
  try:
    max_ini = max(initial_conditions_dict.keys())
  except ValueError:
    max_ini = 1
  select_remove_initial_conditions = i3.number_input("Select to remove:",
                                                      min_value=1,
                                                      max_value=max_ini,
                                                      value=1,
                                                      key="initial-remove")
  remove_initial_conditions_button = i4.button("Remove",
                                               key="initial-remove")
  if remove_initial_conditions_button:
    try:
      to_be_removed = \
        initial_conditions_dict[select_remove_initial_conditions]
      initial_conditions.remove(to_be_removed)
      initial_conditions_dict = {(i+1):r for i,r in
                                 enumerate(initial_conditions)}
    except KeyError:
      pass

  st.markdown("**Your initial conditions:**")
  st.write(initial_conditions_dict)

  # --------------------------------------------------------------------------

  for _ in range(3):
    st.text("")

  # --------------------------------------------------------------------------

  # TERMINATION CONDITIONS

  new_terminal_condition = st.text_input("Add termination condition:")
  t1, t2 = st.beta_columns(2)
  add_terminal_condition_button = t1.button("Add", key="terminal-add")
  if add_terminal_condition_button:
    if new_terminal_condition not in terminal_conditions:
      terminal_conditions.append(new_terminal_condition)
  reset_terminal_conditions = t2.button("Clear all", key="terminal-reset")
  if reset_terminal_conditions:
    terminal_conditions.clear()

  terminal_conditions_dict = {(i+1):ini for i,ini in
                             enumerate(terminal_conditions)}

  t3, t4 = st.beta_columns(2)
  try:
    max_term = max(terminal_conditions_dict.keys())
  except ValueError:
    max_term = 1
  select_remove_terminal_conditions = t3.number_input("Select to remove:",
                                                      min_value=1,
                                                      max_value=max_term,
                                                      value=1,
                                                      key="terminal-remove")
  remove_terminal_conditions_button = t4.button("Remove",
                                                key="terminal-remove")
  if remove_terminal_conditions_button:
    try:
      to_be_removed = \
        terminal_conditions_dict[select_remove_terminal_conditions]
      terminal_conditions.remove(to_be_removed)
      terminal_conditions_dict = {(i+1):r for i,r in
                                  enumerate(terminal_conditions)}
    except KeyError:
      pass

  st.markdown("**Your termination conditions:**")
  st.write(terminal_conditions_dict)

  # --------------------------------------------------------------------------

  for _ in range(3):
    st.text("")

  # --------------------------------------------------------------------------

  # COMPATIBILITY PREDICATES

  st.markdown("**Your compatibility code:**")
  comp_input = st.text_area(label="Introduce compatible/2 predicates:",
                            value="""...\ncompatible(_,[]).""",
                            height=220)
  comp1, comp2 = st.beta_columns(2)
  add_comp_button = comp1.button("Add", key="comp-add")
  clear_comp_button = comp2.button("Clear", key="comp-clear")
  if add_comp_button:
    compatibility_predicates.append(comp_input)
  if clear_comp_button:
    compatibility_predicates.clear()
