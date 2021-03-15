#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 12 09:15:19 2021

@author: nmontes
"""

import tkinter as tk
from tkinter import ttk
from functools import partial
from scrollable import ScrollableFrame
import logging

WIDTH = 50

class StatesTabOne(ScrollableFrame):
  def __init__(self, notebook):
    super().__init__(notebook)
    
    # predicates
    self.new_predicate_label = tk.Label(self.scrollable_frame,
                                        text="New state predicate/arity:")
    self.new_predicate_input = tk.Entry(self.scrollable_frame, width=12)
    self.add_predicate_button = tk.Button(self.scrollable_frame, text="Add",
                                          command=self.add_predicate_widget)
    self.clear_predicate_button = tk.Button(self.scrollable_frame,
                                            text="Clear all",
                                            command=self.clear_all_predicates)
    
    self.new_predicate_label.grid(row=1, column=0)
    self.new_predicate_input.grid(row=1, column=1)
    self.add_predicate_button.grid(row=1, column=2)
    self.clear_predicate_button.grid(row=1, column=3)
    
    self.predicates = {}
    
    # biophysicals
    self.new_biophysical_label = tk.Label(self.scrollable_frame,
                                          text="New biophysical feature:")
    self.new_biophysical_input = tk.Entry(self.scrollable_frame, width=15)
    self.add_biophysical_button = tk.Button(self.scrollable_frame, text="Add",
                                        command=self.add_biophysical_widget)
    self.clear_biophysical_button = tk.Button(self.scrollable_frame,
                                        text="Clear all",
                                        command=self.clear_all_biophysicals)
    
    self.new_biophysical_label.grid(row=1, column=5)
    self.new_biophysical_input.grid(row=1, column=6)
    self.add_biophysical_button.grid(row=1, column=7)
    self.clear_biophysical_button.grid(row=1, column=8)
    
    self.biophysicals = {}
    
    self.vline = ttk.Separator(self.scrollable_frame, orient='vertical')
    self.vline.grid(column=4, row=1, rowspan=99, sticky='ns')
  
  def add_predicate_widget(self):
    new_predicate = self.new_predicate_input.get()
    self.new_predicate_input.delete(0, 'end')
    if new_predicate != '' and new_predicate not in self.predicates.keys():
      predicate_selected = tk.IntVar()
      predicate_checkbox = tk.Checkbutton(self.scrollable_frame,
                                          text=new_predicate,
                                          variable=predicate_selected)
      predicate_checkbox.select()
      remove_new_predicate = partial(self.remove_predicate, new_predicate)
      predicate_remove_button = tk.Button(self.scrollable_frame,
                                          text="Remove",
                                          command=remove_new_predicate)
      self.predicates[new_predicate] = (predicate_checkbox,
                                        predicate_remove_button,
                                        predicate_selected)
      logging.debug("added predicate {}".format(new_predicate))
      logging.debug("predicates: {}\n".format(self.predicates.keys()))
    self.display_predicates_widgets()
    
  def remove_predicate(self, predicate):
    self.predicates[predicate][0].destroy()
    self.predicates[predicate][1].destroy()
    self.predicates.pop(predicate)
    self.display_predicates_widgets()
    logging.debug("removed predicate {}".format(predicate))
    logging.debug("predicates: {}\n".format(self.predicates.keys()))
      
  def display_predicates_widgets(self):   
    row = 2
    for aw in self.predicates.values():
      aw[0].grid(row=row, column=0, sticky='w')
      aw[1].grid(row=row, column=1, sticky='w')
      row += 1
      
  def clear_all_predicates(self):
    for w in self.predicates.values():
      w[0].destroy()
      w[1].destroy()
    self.predicates = {}
    logging.debug("cleared all predicates\n")
    
  def get_predicates(self):
    predicates = []
    for a, ws in self.predicates.items():
      if ws[2].get():
        predicates.append(a)
    logging.debug("Get predicates: {}\n".format(predicates))
    return predicates
  
  def add_biophysical_widget(self):
    new_biophysical = self.new_biophysical_input.get()
    self.new_biophysical_input.delete(0, 'end')
    if new_biophysical != '' and new_biophysical not in \
      self.biophysicals.keys():
      biophysical_selected = tk.IntVar()
      biophysical_checkbox = tk.Checkbutton(self.scrollable_frame,
                                            text=new_biophysical,
                                            variable=biophysical_selected)
      biophysical_checkbox.select()
      remove_new_biophysical = partial(self.remove_biophysical,
                                       new_biophysical)
      biophysical_remove_button = tk.Button(self.scrollable_frame,
                                            text="Remove",
                                            command=remove_new_biophysical)
      self.biophysicals[new_biophysical] = (biophysical_checkbox,
                                            biophysical_remove_button,
                                            biophysical_selected)
      logging.debug("added biophysical {}".format(new_biophysical))
      logging.debug("biophysicals: {}\n".format(self.biophysicals.keys()))
    self.display_biophysicals_widgets()
    
  def remove_biophysical(self, biophysical):
    self.biophysicals[biophysical][0].destroy()
    self.biophysicals[biophysical][1].destroy()
    self.biophysicals.pop(biophysical)
    self.display_biophysicals_widgets()
    logging.debug("removed biophysical {}".format(biophysical))
    logging.debug("biophysicals: {}\n".format(self.biophysicals.keys()))
      
  def display_biophysicals_widgets(self):   
    row = 2
    for aw in self.biophysicals.values():
      aw[0].grid(row=row, column=5, sticky='w')
      aw[1].grid(row=row, column=6, sticky='w')
      row += 1
      
  def clear_all_biophysicals(self):
    for w in self.biophysicals.values():
      w[0].destroy()
      w[1].destroy()
    self.biophysicals = {}
    logging.debug("cleared all biophysicals\n")
    
  def get_biophysicals(self):
    biophysicals = []
    for a, ws in self.biophysicals.items():
      if ws[2].get():
        biophysicals.append(a)
    logging.debug("Get biophysicals: {}\n".format(biophysicals))
    return biophysicals
  


class StatesTabTwo(ScrollableFrame):
  def __init__(self, notebook):
    super().__init__(notebook)
    
    # initial conditions
    self.new_initial_label = tk.Label(self.scrollable_frame,
                                      text="New initial condition:")
    self.new_initial_input = tk.Text(self.scrollable_frame, width=35,
                                     height=3)
    self.new_initial_input.insert(1.0, "initially(...) :- ...")
    self.add_initial_button = tk.Button(self.scrollable_frame, text="Add",
                                        command=self.add_initial_widget)
    self.clear_initial_button = tk.Button(self.scrollable_frame,
                                          text="Clear all",
                                          command=self.clear_all_initials)
    
    self.new_initial_label.grid(row=1, column=0, sticky='ne')
    self.new_initial_input.grid(row=1, column=1)
    self.add_initial_button.grid(row=1, column=2, sticky='nw')
    self.clear_initial_button.grid(row=1, column=3, sticky='nw')
    
    self.initials = {}
    
    # terminals
    self.new_terminal_label = tk.Label(self.scrollable_frame,
                                       text="New termination condition:")
    self.new_terminal_input = tk.Text(self.scrollable_frame, width=35,
                                      height=3)
    self.new_terminal_input.insert(1.0, "terminal :- ...")
    self.add_terminal_button = tk.Button(self.scrollable_frame, text="Add",
                                         command=self.add_terminal_widget)
    self.clear_terminal_button = tk.Button(self.scrollable_frame,
                                           text="Clear all",
                                           command=self.clear_all_terminals)
    
    self.new_terminal_label.grid(row=1, column=5, sticky='ne')
    self.new_terminal_input.grid(row=1, column=6)
    self.add_terminal_button.grid(row=1, column=7, sticky='nw')
    self.clear_terminal_button.grid(row=1, column=8, sticky='nw')
    
    self.terminals = {}
    
    self.vline = ttk.Separator(self.scrollable_frame, orient='vertical')
    self.vline.grid(column=4, row=1, rowspan=99, sticky='ns')
    
  def add_initial_widget(self):
    new_initial = self.new_initial_input.get(1.0, 'end-1c')
    if new_initial != '' and new_initial not in self.initials.keys():
      initial_selected = tk.IntVar()
      initial_checkbox = tk.Checkbutton(self.scrollable_frame,
                                        text=new_initial,
                                        variable=initial_selected,
                                        width=25, height=3, justify="left",
                                        anchor='nw', wraplength=350)
      initial_checkbox.select()
      remove_new_initial = partial(self.remove_initial, new_initial)
      initial_remove_button = tk.Button(self.scrollable_frame, text="Remove",
                                        command=remove_new_initial)
      self.initials[new_initial] = (initial_checkbox,
                                    initial_remove_button,
                                    initial_selected)
      logging.debug("added initial {}".format(new_initial))
      logging.debug("initials: {}\n".format(self.initials.keys()))
    self.display_initials_widgets()
    
  def remove_initial(self, initial):
    self.initials[initial][0].destroy()
    self.initials[initial][1].destroy()
    self.initials.pop(initial)
    self.display_initials_widgets()
    logging.debug("removed initial {}".format(initial))
    logging.debug("initials: {}\n".format(self.initials.keys()))
      
  def display_initials_widgets(self):   
    row = 2
    for aw in self.initials.values():
      aw[0].grid(row=row, column=1, sticky='w')
      aw[1].grid(row=row, column=2, sticky='w')
      row += 1
      
  def clear_all_initials(self):
    for w in self.initials.values():
      w[0].destroy()
      w[1].destroy()
    self.initials = {}
    logging.debug("cleared all initials\n")
    
  def get_initials(self):
    initials = []
    for a, ws in self.initials.items():
      if ws[2].get():
        initials.append(a)
    logging.debug("Get initials: {}\n".format(initials))
    return initials
  
  def add_terminal_widget(self):
    new_terminal = self.new_terminal_input.get(1.0, 'end-1c')
    if new_terminal != '' and new_terminal not in \
      self.terminals.keys():
      terminal_selected = tk.IntVar()
      terminal_checkbox = tk.Checkbutton(self.scrollable_frame,
                                         text=new_terminal,
                                         variable=terminal_selected,
                                         width=25, height=3, justify="left",
                                         anchor='nw', wraplength=350)
      terminal_checkbox.select()
      remove_new_terminal = partial(self.remove_terminal,
                                       new_terminal)
      terminal_remove_button = tk.Button(self.scrollable_frame, text="Remove",
                                            command=remove_new_terminal)
      self.terminals[new_terminal] = (terminal_checkbox,
                                      terminal_remove_button,
                                      terminal_selected)
      logging.debug("added terminal {}".format(new_terminal))
      logging.debug("terminals: {}\n".format(self.terminals.keys()))
    self.display_terminals_widgets()
    
  def remove_terminal(self, terminal):
    self.terminals[terminal][0].destroy()
    self.terminals[terminal][1].destroy()
    self.terminals.pop(terminal)
    self.display_terminals_widgets()
    logging.debug("removed terminal {}".format(terminal))
    logging.debug("terminals: {}\n".format(self.terminals.keys()))
      
  def display_terminals_widgets(self):   
    row = 2
    for aw in self.terminals.values():
      aw[0].grid(row=row, column=6, sticky='w')
      aw[1].grid(row=row, column=7, sticky='w')
      row += 1
      
  def clear_all_terminals(self):
    for w in self.terminals.values():
      w[0].destroy()
      w[1].destroy()
    self.terminals = {}
    logging.debug("cleared all terminals\n")
    
  def get_terminals(self):
    terminals = []
    for a, ws in self.terminals.items():
      if ws[2].get():
        terminals.append(a)
    logging.debug("Get terminals: {}\n".format(terminals))
    return terminals