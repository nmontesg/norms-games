#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar  7 09:30:14 2021

@author: nmontes
"""

import streamlit as st
from build import build_full_game
from extensivegames import plot_game

import app_agents
import app_rules_boundary
import app_rules_position
import app_rules_choice
import app_rules_control
import app_states

st.title("Games from Rules")
st.markdown("""
            This app demonstrates how to model a situation as an Extensive 
            Form Game from the specification of the rules that structure it.
            
            A game is built from three information sources:
              
              1. ``agents.pl`` what agents are there susceptible of entering
              the situation, plus any additional attributes.
              
              2. ``rules.pl`` the rules (of different types) structuring the
              situation.
              
              3. ``states.pl`` initial and termination conditions, plus some 
              auxiliary information.
            """)

PAGES = {
    "agents": app_agents,
    "rules - boundary": app_rules_boundary,
    "rules - position": app_rules_position,
    "rules - choice": app_rules_choice,
    "rules - control": app_rules_control,
    "states": app_states
}
st.sidebar.title('Navigation')
selection = st.sidebar.radio("Go to", list(PAGES.keys()))
page = PAGES[selection]
page.app()



# Game tree layout
st.sidebar.markdown("## Game layout options")
player = st.sidebar.text_input("Agent:", value="alice")
color_options = ['blue', 'green', 'red', 'cyan', 'magenta', 'yellow']
color = st.sidebar.selectbox("Color:", options=color_options)

@st.cache(allow_output_mutation=True)
def cached_colors():
  return {}

player_colors = cached_colors()
add_color = st.sidebar.button("Add")
if add_color:
  player_colors.update({player: color})
reset_colors = st.sidebar.button("Clear all")
if reset_colors:
  player_colors.clear()
st.sidebar.write(player_colors)

rule_identifier = st.sidebar.text_input("Rule identifier")
rule_threshold = st.sidebar.number_input("Max. rule priority",
                                         min_value=0,
                                         max_value=10,
                                         value=0)
max_rounds = st.sidebar.number_input("Max. rounds",
                                  min_value=1,
                                  max_value=20,
                                  value=10)

build_button = st.sidebar.button("BUILD GAME")

my_fig_kwargs = dict(figsize=(25,15), frameon=False, tight_layout=True)
my_node_kwargs = dict(font_size=10, node_size=500, edgecolors='k',
                      linewidths=1.5)
my_edge_kwargs = dict(arrowsize=15, width=2.5)
my_edge_labels_kwargs = dict(font_size=14)
my_patch_kwargs = dict(linewidth=1.5)
my_legend_kwargs = dict(fontsize=16, loc='upper right', edgecolor='white')
my_utility_label_kwargs = dict(horizontalalignment='center',
                                fontsize=10)
my_info_sets_kwargs = dict(linestyle='--', linewidth=2)

@st.cache
def make_game(rule_identifier, rule_threshold, max_rounds):
  game = build_full_game(".", identifier=rule_identifier,
                         threshold=rule_threshold, max_rounds=max_rounds,
                         verbose=True)
  return game

if build_button:
  # write agents.pl file
  agents_file = open("agents.pl", "w+")
  agents_file.close()
  agents_file = open("agents.pl", "a+")
  agents, agent_attributes = app_agents.agents, app_agents.agent_attributes
  for a in agents:
    agents_file.write("agent({}).\n".format(a))
  agents_file.write("\n")
  for at in agent_attributes:
    agents_file.write("{}.\n".format(at))
  agents_file.close()
  
  # write rules.pl file
  all_rules = app_rules_boundary.boundary_rules + \
    app_rules_position.position_rules + \
    app_rules_choice.choice_rules + \
    app_rules_control.control_rules
  rules_file = open("rules.pl", "w+")
  rules_file.close()
  rules_file = open("rules.pl", "a+")
  rules_file.write(":- use_module(library(clpr)).\n\n")
  for r in all_rules:
    rules_file.write("{}\n\n".format(r))
    
  rules_file.close()
  
  # write states.pl file
  states_file = open("states.pl", "w+")
  states_file.close()
  states_file = open("states.pl", "a+")
  
  dynamic_predicates = app_states.dynamic_predicates
  states_file.write("dynamic :- {}.".format(', '.join(dynamic_predicates)))
  states_file.write("\n\n")
  
  biophysical_features = app_states.biophysical_features
  for b in biophysical_features:
    states_file.write("{}.\n".format(b))
  states_file.write("\n")
  
  initial_conditions = app_states.initial_conditions
  for ini in initial_conditions:
    states_file.write("{}.\n".format(ini))
  states_file.write("\n")
  
  termination_conditions = app_states.terminal_conditions
  for t in termination_conditions:
    states_file.write("terminal :- {}.\n".format(t))
  states_file.write("\n")

  compatibility_predicates = app_states.compatibility_predicates[-1]
  states_file.write("{}\n".format(compatibility_predicates))
  states_file.close()
  
  game = make_game(rule_identifier, rule_threshold, max_rounds)

  # fig = plot_game(game,
  #                 player_colors,
  #                 fig_kwargs=my_fig_kwargs,
  #                 node_kwargs=my_node_kwargs,
  #                 edge_kwargs=my_edge_kwargs,
  #                 edge_labels_kwargs=my_edge_labels_kwargs,
  #                 patch_kwargs=my_patch_kwargs,
  #                 legend_kwargs=my_legend_kwargs,
  #                 draw_utility=False,
  #                 info_sets_kwargs=my_info_sets_kwargs)

  # st.write(fig)
  