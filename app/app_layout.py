#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 12 11:48:00 2021

@author: nmontes
"""

import tkinter as tk
from tkinter import ttk
from tkinter import colorchooser
from functools import partial
from scrollable import ScrollableFrame
import logging

class LayoutTab(ScrollableFrame):
  def __init__(self, notebook):
    super().__init__(notebook)
    
    self.player_colors = {}
    
    self.new_color_button = tk.Button(self.scrollable_frame, text='New color',
                                      command=self.new_color_widgets)
    self.new_color_button.grid(row=1, column=0)
    
    
  def new_color_widgets(self):
    new_color = colorchooser.askcolor()
    agent_entry = tk.Entry(self.scrollable_frame, width=12)
    color_chosen_display = tk.Label(self.scrollable_frame, bg=new_color[1],
                                    width=5)
    remove_color_func = partial(self.remove_color, new_color[1])
    remove_button = tk.Button(self.scrollable_frame, text='Remove',
                              command=remove_color_func)
    self.player_colors[new_color[1]] = (agent_entry, color_chosen_display,
                                        remove_button)
    logging.debug("added color {}".format(new_color[1]))
    self.display_colors_widgets()
        
  def display_colors_widgets(self):
    row = 2
    for entry, color_button, remove_button in self.player_colors.values():
      entry.grid(row=row, column=1)
      color_button.grid(row=row, column=2)
      remove_button.grid(row=row, column=3)
      row += 1
    print(self.get_player_colors())
      
  def remove_color(self, color):
    self.player_colors[color][0].destroy()
    self.player_colors[color][1].destroy()
    self.player_colors.pop(color)
    self.display_colors_widgets()
    logging.debug("removed color {}".format(color))
    
  def get_player_colors(self):
    player_colors = {}
    for color, w in self.player_colors.items():
      agent = w[0].get()
      if agent != '':
        player_colors[agent] = color
    return player_colors
    
    
