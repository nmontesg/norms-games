#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 12 12:08:06 2021

@author: nmontes
"""

import tkinter as tk
from tkinter import ttk


class ScrollableFrame(ttk.Frame):
  def __init__(self, container, *args, **kwargs):
    super().__init__(container, *args, **kwargs)
    canvas = tk.Canvas(self, highlightthickness=0)
    scrollbar = ttk.Scrollbar(self, orient="vertical", command=canvas.yview)
    self.scrollable_frame = ttk.Frame(canvas)

    self.scrollable_frame.bind(
        "<Configure>",
        lambda e: canvas.configure(
            scrollregion=canvas.bbox("all")
        )
    )

    canvas.create_window((0, 0), window=self.scrollable_frame, anchor="nw")

    canvas.configure(yscrollcommand=scrollbar.set)

    canvas.pack(side="left", fill="both", expand=True)
    scrollbar.pack(side="right", fill="y")
