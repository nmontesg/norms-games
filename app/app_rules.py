#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 11 13:12:19 2021

@author: nmontes
"""

import tkinter as tk
from tkinter import ttk
from functools import partial
from scrollable import ScrollableFrame
import logging

WIDTH = 50
HEIGHT = 5

class RulesTab(ScrollableFrame):
  def __init__(self, notebook):
    super().__init__(notebook)
      
    self.new_rule_label = tk.Label(self.scrollable_frame, text="New rule:",
                                   font=('Helvetica', 14, 'bold'))
        
    self.rule_type_label = tk.Label(self.scrollable_frame, text="Type")
    self.rule_type_option = ttk.Combobox(self.scrollable_frame, width=10)
    self.rule_type_option['values'] = ('boundary', 'position', 'choice',
                                       'control')
    self.rule_type_option['state'] = 'readonly'
    self.rule_type_option.bind('<<ComboboxSelected>>', self.template)
    self.rule_type_option.set('boundary')
    
    self.priority_label = tk.Label(self.scrollable_frame, text="Priority")
    self.priority_input = ttk.Combobox(self.scrollable_frame, width=3)
    self.priority_input.set(0)
    self.priority_input['state'] = 'readonly'
    self.priority_input['values'] = tuple(i for i in range(11))
    
    self.if_label = tk.Label(self.scrollable_frame, text="if",
                             font=('Courier', 14))
    self.if_entry = tk.Text(self.scrollable_frame, width=WIDTH, height=HEIGHT)
    self.if_entry.insert(1.0, "agent(A) and ...")
    
    self.then_label = tk.Label(self.scrollable_frame, text="then",
                               font=('Courier', 14))
    self.then_entry = tk.Text(self.scrollable_frame, width=WIDTH,
                              height=HEIGHT)
    self.then_entry.insert(1.0, "(~)participates(A)")
    
    self.where_label = tk.Label(self.scrollable_frame, text="where",
                                font=('Courier', 14))
    self.where_entry = tk.Text(self.scrollable_frame, width=WIDTH,
                               height=HEIGHT)
        
    self.new_rule_label.grid(row=1, column=0)
    self.rule_type_label.grid(row=3, column=0, sticky='e')
    self.rule_type_option.grid(row=3, column=1, sticky='w')
    self.priority_label.grid(row=4, column=0, sticky='e')
    self.priority_input.grid(row=4, column=1, sticky='w')
    self.if_label.grid(row=5, column=0, sticky='ne')
    self.if_entry.grid(row=5, column=1, sticky='w')
    self.then_label.grid(row=6, column=0, sticky='ne')
    self.then_entry.grid(row=6, column=1, sticky='w')
    self.where_label.grid(row=7, column=0, sticky='ne')
    self.where_entry.grid(row=7, column=1, sticky='w')
    
    self.add_rule_button = tk.Button(self.scrollable_frame, text='Add',
                                     command=self.add_rule)
    self.clear_rules_button = tk.Button(self.scrollable_frame, text='Clear all',
                                        command=self.clear_all_rules)
    
    self.add_rule_button.grid(row=8, column=1)
    self.clear_rules_button.grid(row=9, column=1)
    
    self.rules = {}
    
    self.compatible_code_label = tk.Label(self.scrollable_frame,
                                          text="compatible/2 code:",
                                          font=('Helvetica', 14, 'bold'))
    self.compatible_code = tk.Text(self.scrollable_frame,
                                   width=WIDTH, height=3*HEIGHT)
    self.compatible_code.insert(1.0, "...\ncompatible(_,[]).")
    self.compatible_code_label.grid(row=1, column=9, sticky='ne')
    self.compatible_code.grid(row=1, column=10, rowspan=5)
    
    self.vline = ttk.Separator(self.scrollable_frame, orient='vertical')
    self.vline.grid(column=8, row=1, rowspan=99, sticky='ns')
    

  def template(self, _):
    if self.rule_type_option.get() == 'boundary':
      self.if_entry.delete(1.0, 'end-1c')
      self.if_entry.insert(1.0, "agent(A) and ...")
      self.then_entry.delete(1.0, 'end-1c')
      self.then_entry.insert(1.0, "(~)participates(A)")
      
    if self.rule_type_option.get() == 'position':
      self.if_entry.delete(1.0, 'end-1c')
      self.if_entry.insert(1.0, "participates(A) and ...")
      self.then_entry.delete(1.0, 'end-1c')
      self.then_entry.insert(1.0, "(~)role(A, ...)")
      
    if self.rule_type_option.get() == 'choice':
      self.if_entry.delete(1.0, 'end-1c')
      self.if_entry.insert(1.0, "role(A, ...) and ...")
      self.then_entry.delete(1.0, 'end-1c')
      self.then_entry.insert(1.0, "(~)can(A, ...)")
    
    
    if self.rule_type_option.get() == 'control':
      self.if_entry.delete(1.0, 'end-1c')
      self.if_entry.insert(1.0, "does(A, ...) and ...")
      self.then_entry.delete(1.0, 'end-1c')
      self.then_entry.insert(1.0, "(~)... withProb ...")
      
      
  def add_rule(self):
    new_rule_string = self.build_rule_string()
    checkbox_text = new_rule_string
    rule_selected = tk.IntVar()
    rule_checkbox = tk.Checkbutton(self.scrollable_frame, text=checkbox_text,
                                   variable=rule_selected, width=WIDTH-10,
                                   height=3, justify="left", anchor='nw',
                                   wraplength=350)
    rule_checkbox.select()
    remove_new_rule = partial(self.remove_rule, new_rule_string)
    rule_remove_button = tk.Button(self.scrollable_frame, text="Remove",
                                   command=remove_new_rule)
    self.rules[new_rule_string] = (rule_checkbox, rule_remove_button,
                                   rule_selected)
    logging.debug("added rule: {}\n".format(new_rule_string))
    self.display_rule_widgets()
      
  
  def build_rule_string(self):
    rule_type = self.rule_type_option.get()
    rule_priority = self.priority_input.get()
    rule_if = self.if_entry.get(1.0, 'end-1c')
    rule_then = self.then_entry.get(1.0, 'end-1c')
    rule_where = self.where_entry.get(1.0, 'end-1c')
    if rule_type == 'control':
      rule_string = "rule(id, {}, {}, if {} then [{}] where [{}]).".\
        format(rule_type, rule_priority, rule_if, rule_then,
               rule_where)
    else:
      rule_string = "rule(id, {}, {}, if {} then {} where [{}]).".\
        format(rule_type, rule_priority, rule_if, rule_then,
               rule_where)
    return rule_string
  
  def remove_rule(self, rule):
    self.rules[rule][0].destroy()
    self.rules[rule][1].destroy()
    self.rules.pop(rule)
    self.display_rule_widgets()
    logging.debug("removed rule: {}\n".format(rule))
    
  def display_rule_widgets(self):
    row = 10
    for rw in self.rules.values():
      rw[0].grid(row=row, column=1, sticky='w')
      rw[1].grid(row=row, column=2, sticky='w')
      row += 1
      
  def clear_all_rules(self):
    for w in self.rules.values():
      w[0].destroy()
      w[1].destroy()
    self.rules = {}
    logging.debug("cleared all rules\n")
    
  def get_rules(self):
    rules = []
    for r, ws in self.rules.items():
      if ws[2].get():
        rules.append(r)
    logging.debug("Get rules: {}\n".format(rules))
    return rules
      