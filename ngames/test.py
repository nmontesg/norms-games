#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr  3 19:45:51 2021

@author: nmontes
"""

import matplotlib.pyplot as plt
from extensivegames import ExtensiveFormGame, plot_game
from build import build_full_game
from equilibrium import build_subgame, minimize_incentives, subgame_perfect_equilibrium

def set_utility(game):
  r"""Set the utility of the game at the terminal nodes.
  
  Set the utility at the terminal nodes to the payoff the agents have
  received. Other possibilities are possible (e.g. consider equality).
  """
  for n in game.game_tree.terminal_nodes:
    node_utility = {}
    facts = game.node_info[n]
    for f in facts:
      predicate = f.split('(')[0]
      args = f.split('(')[1][:-1].split(', ')
      if predicate == 'payoff':
        node_utility[args[0]] = float(args[1])
    game.set_utility(n, node_utility)
    

folder = '/home/nmontes/OneDrive/Documentos/PhD/ngames/examples/gamesFromRules/axelrod-metanorms'
game = build_full_game(folder, 'metanorms', threshold=0)
set_utility(game)

my_fig_kwargs = dict(figsize=(15, 19), frameon=False)
my_node_kwargs = dict(font_size=30, node_size=2250, edgecolors='k',
                      linewidths=2)
my_edge_kwargs = dict(arrowsize=25, width=3)
my_edge_labels_kwargs = dict(font_size=20)
my_patch_kwargs = dict(linewidth=2)
my_legend_kwargs = dict(fontsize=24, loc='upper right', edgecolor='white')
my_utility_label_kwargs = dict(horizontalalignment='center', fontsize=20)
my_info_sets_kwargs = dict(linestyle='--', linewidth=3)

position_colors =  {'i':'aquamarine', 'j':'greenyellow', 'k':'violet'}

fig = plot_game(game,
                position_colors,
                fig_kwargs=my_fig_kwargs,
                node_kwargs=my_node_kwargs,
                edge_kwargs=my_edge_kwargs,
                edge_labels_kwargs=my_edge_labels_kwargs,
                patch_kwargs=my_patch_kwargs,
                legend_kwargs=my_legend_kwargs,
                utility_label_kwargs=my_utility_label_kwargs,
                utility_label_shift=0.06,
                info_sets_kwargs=my_info_sets_kwargs)

nfg = build_subgame(game, 4)
print(nfg.players)
print(nfg.player_actions)
print(nfg.payoffs)