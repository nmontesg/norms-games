#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Apr 23 10:09:30 2021

@author: nmontes
"""

from pyswip import Prolog
from pathlib import Path
import networkx as nx
from extensivegames import ExtensiveFormGame
from typing import Any, Set, Dict


prolog = Prolog()








class StochasticGame:
  def __init__(self, **kwargs) -> None:
    # set of players
    self.players = set()
    
    # game tree
    self.tree = nx.DiGraph()
    self.tree.root = None
    self.tree.terminal_states = {}
    
    # additional info
    for k, v in kwargs.items():
      setattr(self, k, v)
      
  def add_players(self, *players: Any) -> None:
    for p in players:
      if p == 'chance':
        raise ValueError("player 'chance' should not added to the game")
      self.players.add(p)
      
  def add_state(self, facts: Set[str], is_root: bool=False,
                is_terminal: bool=False) -> None:
    try:
      state_int = max(self.tree.nodes) + 1
    except ValueError:
      state_int = 1
    self.tree.add_node(state_int, fluents=facts)
    
  def set_state_round_game(self, state: int,
                           game_round: ExtensiveFormGame) -> None:
    self.tree.nodes[state]['game'] = game_round

  def add_edge(self, from_state: int, to_state: int,
               action_profile: Dict[str,str], probability: float=1) -> None:
    self.tree.add_edge(from_state, to_state, joint_action=action_profile,
                       p=probability)
    