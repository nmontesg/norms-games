#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 10 14:54:06 2021

@author: nmontes
"""

import os, sys
currentdir = os.path.dirname(os.path.realpath(__file__))
parentdir = os.path.dirname(currentdir)
sys.path.append(parentdir+'/ngames')
import logging
logging.getLogger().setLevel(level=logging.DEBUG)
import build
from extensivegames import plot_game
from pyswip import Prolog

import tkinter as tk
from tkinter import ttk
from tkinter import messagebox
from app_agents import AgentsTab
from app_rules import RulesTab
from app_states import StatesTabOne, StatesTabTwo
from app_layout import LayoutTab

root = tk.Tk()
root.title("Games from Rules")
width = root.winfo_screenwidth()
height = root.winfo_screenheight()
root.geometry("{:d}x{:d}".format(width*3//5, height*3//5))
root.resizable(True, True)

notebook = ttk.Notebook(root)
agents_tab = AgentsTab(notebook)
rules_tab = RulesTab(notebook)
states_tab_1 = StatesTabOne(notebook)
states_tab_2 = StatesTabTwo(notebook)
layout_tab = LayoutTab(notebook)


notebook.add(agents_tab, text='Agents')
notebook.add(rules_tab, text='Rules')
notebook.add(states_tab_1, text='States (1)')
notebook.add(states_tab_2, text = 'States (2)')
notebook.add(layout_tab, text = 'Layout')
notebook.pack(expand=1, fill='both')


def build_prolog_files():
  # write agents.pl file
  agents = agents_tab.get_agents()
  agent_attributes = agents_tab.get_attributes()
  agents_file = open("agents.pl", "a+")
  for a in agents:
    agents_file.write("agent({}).\n".format(a))
  agents_file.write("\n")
  for at in agent_attributes:
    agents_file.write("{}\n".format(at))
  agents_file.close()
  logging.debug("agents.pl built successfully")
  
  # write rules.pl file
  all_rules = rules_tab.get_rules()
  rules_file = open("rules.pl", "a+")
  rules_file.write(":- use_module(library(clpr)).\n\n")
  for r in all_rules:
    rules_file.write("{}\n\n".format(r))
  rules_file.close()
  logging.debug("rules.pl built successfully")
  
  # write states.pl file
  states_file = open("states.pl", "a+")
  
  dynamic_predicates = states_tab_1.get_predicates()
  states_file.write(":- dynamic {}.".format(', '.join(dynamic_predicates)))
  states_file.write("\n\n")
  
  biophysical_features = states_tab_1.get_biophysicals()
  for b in biophysical_features:
    states_file.write("{}.\n".format(b))
  states_file.write("\n")
  
  initial_conditions = states_tab_2.get_initials()
  for ini in initial_conditions:
    states_file.write("{}.\n".format(ini))
  states_file.write("\n")
  
  termination_conditions = states_tab_2.get_terminals()
  for t in termination_conditions:
    states_file.write("{}.\n".format(t))
  states_file.write("\n")

  compatibility_code = rules_tab.compatible_code.get(1.0, 'end-1c')
  states_file.write("{}\n".format(compatibility_code))
  states_file.close()
  logging.debug("states.pl built successfully")


def build_game():
  build_prolog_files()
  logging.debug("Prolog files built successfully")
  
  # reset prolog
  build.prolog = Prolog()
  
  # build game
  try:
    game = build.build_full_game('.', identifier='id')
    player_colors = layout_tab.get_player_colors()

    # default keywords for rendering the figure
    my_fig_kwargs = dict(figsize=(10,8), frameon=False, tight_layout=True)
    my_node_kwargs = dict(font_size=10, node_size=500, edgecolors='k',
                          linewidths=1.5)
    my_edge_kwargs = dict(arrowsize=15, width=2.5)
    my_edge_labels_kwargs = dict(font_size=8)
    my_patch_kwargs = dict(linewidth=1.5)
    my_legend_kwargs = dict(fontsize=16, loc='upper right', edgecolor='white')
    my_info_sets_kwargs = dict(linestyle='--', linewidth=2)
    fig = plot_game(game, 
                    player_colors,
                    fig_kwargs=my_fig_kwargs,
                    node_kwargs=my_node_kwargs,
                    edge_kwargs=my_edge_kwargs,
                    edge_labels_kwargs=my_edge_labels_kwargs,
                    patch_kwargs=my_patch_kwargs,
                    legend_kwargs=my_legend_kwargs,
                    draw_utility=False,
                    info_sets_kwargs=my_info_sets_kwargs)
    fig.show()
    
  except:
    messagebox.showerror("Error", "The game could not be built")

  finally:
    # delete prolog files
    if os.path.isfile('agents.pl'):
      os.remove('agents.pl')
    if os.path.isfile('rules.pl'):
      os.remove('rules.pl')
    if os.path.isfile('states.pl'):
      os.remove('states.pl')


build_button = tk.Button(layout_tab.scrollable_frame, text="Build Game",
                         font=('Helvetica', 14, 'bold'), command=build_game)
build_button.grid(row=1, column=1)


if __name__ == '__main__':
  root.mainloop()
