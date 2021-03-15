#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 11 11:46:54 2021

@author: nmontes
"""

import tkinter as tk
from tkinter import ttk
from functools import partial
from scrollable import ScrollableFrame
import logging

class AgentsTab(ScrollableFrame):
  def __init__(self, notebook):
    super().__init__(notebook)
    
    # agents
    self.new_agent_label = tk.Label(self.scrollable_frame, text="New agent:")
    self.new_agent_input = tk.Entry(self.scrollable_frame, width=10)
    self.add_agent_button = tk.Button(self.scrollable_frame, text="Add",
                                      command=self.add_agent_widget)
    self.clear_agent_button = tk.Button(self.scrollable_frame,
                                        text="Clear all",
                                        command=self.clear_all_agents)
    
    self.new_agent_label.grid(row=1, column=0, sticky='e')
    self.new_agent_input.grid(row=1, column=1)
    self.add_agent_button.grid(row=1, column=2)
    self.clear_agent_button.grid(row=1, column=3)
    
    self.agents = {}
    
    # agent attributes
    self.new_attribute = tk.Label(self.scrollable_frame,
                                  text="New agent attribute:")
    self.new_attribute_input = tk.Entry(self.scrollable_frame, width=12)
    self.new_attribute_agent= ttk.Combobox(self.scrollable_frame, width=10)
    self.new_attribute_agent['state'] = 'readonly'
    self.new_attribute_value = tk.Entry(self.scrollable_frame, width=5)
    self.add_attribute_button = tk.Button(self.scrollable_frame, text="Add",
                                          command=self.add_attribute_widget)
    self.clear_attribute_button = tk.Button(self.scrollable_frame,
                                            text="Clear all",
                                            command=self.clear_all_attributes)
    
    
    self.new_attribute.grid(row=1, column=5, sticky='e')
    self.new_attribute_input.grid(row=1, column=6)
    self.new_attribute_agent.grid(row=1, column=7)
    self.new_attribute_value.grid(row=1, column=8)
    self.add_attribute_button.grid(row=1, column=9)
    self.clear_attribute_button.grid(row=1, column=10)
    
    self.attributes = {}
    
    self.vline = ttk.Separator(self.scrollable_frame, orient='vertical')
    self.vline.grid(column=4, row=1, rowspan=99, sticky='ns')
  
  def add_agent_widget(self):
    new_agent = self.new_agent_input.get()
    self.new_agent_input.delete(0, 'end')
    if new_agent != '' and new_agent not in self.agents.keys():
      agent_selected = tk.IntVar()
      agent_checkbox = tk.Checkbutton(self.scrollable_frame, text=new_agent,
                                      variable=agent_selected)
      agent_checkbox.select()
      remove_new_agent = partial(self.remove_agent, new_agent)
      agent_remove_button = tk.Button(self.scrollable_frame, text="Remove",
                                      command=remove_new_agent)
      self.agents[new_agent] = (agent_checkbox, agent_remove_button,
                                agent_selected)
      logging.debug("added agent {}".format(new_agent))
      logging.debug("agents: {}\n".format(self.agents.keys()))
    self.display_agents_widgets()
    self.new_attribute_agent['values'] = list(self.agents.keys())
    
  def remove_agent(self, agent):
    self.agents[agent][0].destroy()
    self.agents[agent][1].destroy()
    self.agents.pop(agent)
    self.display_agents_widgets()
    self.new_attribute_agent['values'] = list(self.agents.keys())
    logging.debug("removed agent {}".format(agent))
    logging.debug("agents: {}\n".format(self.agents.keys()))
      
  def display_agents_widgets(self):   
    row = 2
    for aw in self.agents.values():
      aw[0].grid(row=row, column=0, sticky='w')
      aw[1].grid(row=row, column=1, sticky='w')
      row += 1
      
  def clear_all_agents(self):
    for w in self.agents.values():
      w[0].destroy()
      w[1].destroy()
    self.agents = {}
    self.new_attribute_agent['values'] = list(self.agents.keys())
    logging.debug("cleared all agents\n")
    
  def get_agents(self):
    agents = []
    for a, ws in self.agents.items():
      if ws[2].get():
        agents.append(a)
    logging.debug("Get agents: {}\n".format(agents))
    return agents
  
  def get_attribute(self):
    attribute_name = self.new_attribute_input.get()
    agent = self.new_attribute_agent.get()
    value = self.new_attribute_value.get()
    if attribute_name != '' and agent != '' and value != '':
      attribute = "{}({},{}).".format(attribute_name, agent, value)
      return attribute
  
  def add_attribute_widget(self):
    new_attribute = self.get_attribute()
    if new_attribute is None:
      return
    if new_attribute not in self.attributes.keys():
      attribute_selected = tk.IntVar()
      attribute_checkbox = tk.Checkbutton(self.scrollable_frame,
                                          text=new_attribute,
                                          variable=attribute_selected)
      attribute_checkbox.select()
      remove_new_attribute = partial(self.remove_attribute, new_attribute)
      attribute_remove_button = tk.Button(self.scrollable_frame,
                                          text="Remove",
                                          command=remove_new_attribute)
      self.attributes[new_attribute] = (attribute_checkbox,
                                        attribute_remove_button,
                                        attribute_selected)
      logging.debug("added attribute {}".format(new_attribute))
      logging.debug("attributes: {}\n".format(self.attributes.keys()))
    self.display_attribute_widgets()
    
  def remove_attribute(self, attribute):
    self.attributes[attribute][0].destroy()
    self.attributes[attribute][1].destroy()
    self.attributes.pop(attribute)
    self.display_attribute_widgets()
    logging.debug("removed attribute {}".format(attribute))
    logging.debug("attributes: {}\n".format(self.attribute.keys()))
      
  def display_attribute_widgets(self):   
    row = 2
    for aw in self.attributes.values():
      aw[0].grid(row=row, column=5, sticky='w')
      aw[1].grid(row=row, column=6, sticky='w')
      row += 1
      
  def clear_all_attributes(self):
    for w in self.attributes.values():
      w[0].destroy()
      w[1].destroy()
    self.attributes = {}
    logging.debug("cleared all attributes\n")
    
  def get_attributes(self):
    attributes = []
    for a, ws in self.attributes.items():
      if ws[2].get():
        attributes.append(a)
    logging.debug("Get attributes: {}\n".format(attributes))
    return attributes
  