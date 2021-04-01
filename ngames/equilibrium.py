#!/usr/bin/env python3# -*- coding: utf-8 -*-r"""Compute equilibrium paths of play using backward induction.Assume sub-game perfect rationality.To be applied on extensive-form games that are equivalent to heterogenousnormal form games played sequentially, i.e. games that have been generatedfrom a set of Prolog files and the ``build.build_full_game`` function."""from networkx.algorithms.dag import descendantsfrom extensivegames import ExtensiveFormGamefrom normalgames import NormalFormGamefrom typing import Callable, Dict, Any, Tuplefrom itertools import product__author__ = "Nieves Montes"__copyright__ = "Copyright 2020, Nieves Montes"__credits__ = ["Nieves Montes"]__license__ = "MIT"__version__ = "0.0.1"__maintainer__ = "Nieves Montes"__email__ = "nmontes@iiia.csic.es"__status__ = "Prototype"def build_subgame(extensive_game: ExtensiveFormGame, root: int) \  -> NormalFormGame:  r"""Build a normal form game that emanates from a node in an extensive game.    Assumes that the root from which the normal form game is built corresponds  to the root of the last round of the extensive game.  Parameters  ----------  extensive_game : ExtensiveFormGame  root : int    The root from which a last rounds of the extensive game starts.  Raises  ------  AssertionError    If a descendant of a chance node is not terminal.  ValueError    If a path of play does not ends at a chance or at a terminal node.  Returns  -------  normal_form_game : NormalFormGame    The game in normal form corresponding to the .  """  # get the players and their possible actions  player_actions = {}  n = root  while True:    try:      turn = extensive_game.turn_function[n]      if turn == 'chance':        break    # KeyError happens if terminal node is reached, not in the turn function    except KeyError:      break      active_player = extensive_game.turn_function[n]    out_edges = list(extensive_game.game_tree.out_edges(n, data='action'))    actions = tuple(o[2] for o in out_edges)    player_actions[active_player] = actions    n = out_edges[0][1]      players = list(player_actions.keys())  actions = [player_actions[p] for p in players]    # build the payoff function  payoff_function = {}  possible_play_paths = product(*actions)    # scan through the possible paths of play  for p in possible_play_paths:    n = root    for action in p:      out_edges = list(extensive_game.game_tree.out_edges(n, data='action'))      next_node = [o[1] for o in out_edges if o[2] == action][0]      n = next_node          # last node of play path is a terminal node    if n in extensive_game.game_tree.terminal_nodes:      utility = tuple(extensive_game.utility[n][p] for p in players)    # last node of play path is a chance node: weight average over    # descendant terminal nodes    elif extensive_game.turn_function[n] == 'chance':      utility_dict = {pl: 0 for pl in players}      for (_, desc), prob in extensive_game.probability[n].items():        assert desc in extensive_game.game_tree.terminal_nodes, "node {} \          should be a terminal node".format(desc)        for pl in players:          utility_dict[pl] += prob*extensive_game.utility[desc][pl]      utility = tuple(utility_dict[pl] for pl in players)    else:      raise ValueError("node at end of path play {} from root node {} is not \                       a terminal nor a chance node".format(p, root))        payoff_function[p] = utility    normal_form_game = NormalFormGame(    players = players,    actions=actions,    payoff_function=payoff_function    )    return normal_form_gamedef uniform_strategy(normal_game: NormalFormGame) \  -> Dict[Any, Dict[Any, float]]:  r"""Return the perfectly mixed strategy over a normal form game.    This is not a realistic equilibrium strategy computation, just for debugging  purposes.    Parameters  ----------  normal_game : NormalFormGame  Returns  -------  Dict[Any, Dict[Any, float]]    The mixed strategy that randomizes over all available actions for all    players.  """  strategy = {}  for n, p in enumerate(normal_game.players):    num_actions = len(normal_game.player_actions[n])         strategy[p] = {a: 1/num_actions for a in normal_game.player_actions[n]}  return strategy  def subgame_perfect_equilibrium(extensive_game: ExtensiveFormGame, \  equilibrium_function: Callable) -> Tuple[Dict[Any, Any], Dict[Any, Any]]:  r"""Compute the sequential equilibriums in an Extensive Form Game.    This function works on an Extensive Form Game built as a sequence of,  possibly different, Normal Form Games. This function identifies the subgames  starting from those closer to the end of the game (i.e. to the root nodes),  and passes them to ``build_subgame`` to build that part of the game tree as  a normal form game. Then, it calls the provided equilibrium function to  find the mixed strategy incentivized in that subgame, and backtracks the  resulting utility up the game tree.    Parameters  ----------  extensive_game : ExtensiveFormGame  equilibrium_function : Callable    The function that computes a solution concept on a (single-rounds) normal    form game. It should return the result as a dictionary with the game    players as keys, and a dictionary mapping their available actions to the    probabilities as values.  Returns  -------  subgame_mixed_strategies : Dict[Any, Any]    The mapping from the nodes that are the roots of the subgames to the mixed    equilibrium strategies computed for their subgame.  backtrack_utilities : Dict[Any, Any]    The mapping from the nodes that are the roots of the subgames to the    utilities resulting from the computed equilibrium strategies.  """  max_rounds = max(extensive_game.node_rounds.values())  subgame_mixed_strategies = {}  backtrack_utilities = {}  subgame_rounds = max_rounds-1    while subgame_rounds >= 0:    subgame_root_nodes = [n for n in extensive_game.node_rounds.keys() \      if extensive_game.node_rounds[n] == subgame_rounds and      n not in extensive_game.game_tree.terminal_nodes]        # compute equilibrium at the subgames closest to the terminal nodes    for s in subgame_root_nodes:      # build the last subgame as an extensive form game      normal_game = build_subgame(extensive_game, s)            # TODO: function to compute the mixed strategy equilibrium here      mixed_equilibrium_strategy = equilibrium_function(normal_game)            subgame_mixed_strategies[s] = mixed_equilibrium_strategy            # store utility at the root node of the subgame      mes_list = [mixed_equilibrium_strategy[p] for p in normal_game.players]      rewards = normal_game.mixed_strategies_rewards(*mes_list)      backtrack_utilities[s] = {p: r for p, r in \                                zip(normal_game.players, rewards)}        for s in subgame_root_nodes:      # delete all descendants from subgame root nodes      desc = descendants(extensive_game.game_tree, s)      extensive_game.game_tree.remove_nodes_from(desc)            # remove previous terminal nodes from terminal list      terminal_previous = [d for d in desc \                           if d in extensive_game.game_tree.terminal_nodes]      for t in terminal_previous:        extensive_game.game_tree.terminal_nodes.remove(t)      extensive_game.game_tree.terminal_nodes.append(s)            # remove previous non-terminal nodes from turn function      for d in desc:        if d in extensive_game.turn_function.keys():          extensive_game.turn_function.pop(d)                # remove subgame root node from turn function because now it is terminal      extensive_game.turn_function.pop(s)            # backtrack utility      extensive_game.set_utility(s, backtrack_utilities[s])    subgame_rounds -= 1      return subgame_mixed_strategies, backtrack_utilities    if __name__ == '__main__':  from numpy.random import randint  from extensivegames import ExtensiveFormGame, plot_game  from build import build_full_game    game = build_full_game('/home/nmontes/OneDrive/Documentos/PhD/ngames/examples/gamesFromRules/fishing-game', 'fishers', threshold=0)    for t in game.game_tree.terminal_nodes:    u = {p: randint(10) for p in game.players}    game.utility[t] = u    my_fig_kwargs = dict(figsize=(20,14), frameon=False)      my_fig_kwargs = dict(figsize=(20,10), frameon=False)  my_node_kwargs = dict(font_size=10, node_size=500, edgecolors='k',                        linewidths=1.5)  my_edge_kwargs = dict(arrowsize=15, width=1.5)  my_edge_labels_kwargs = dict(font_size=10)  my_patch_kwargs = dict(linewidth=2)  my_legend_kwargs = dict(fontsize=16, loc='upper right', edgecolor='white')  my_utility_label_kwargs = dict(horizontalalignment='center', fontsize=10)  my_info_sets_kwargs = dict(linestyle='--', linewidth=3)    position_colors =  {'alice':'aquamarine', 'bob':'greenyellow'}    fig = plot_game(game,                  position_colors,                  fig_kwargs=my_fig_kwargs,                  node_kwargs=my_node_kwargs,                  edge_kwargs=my_edge_kwargs,                  edge_labels_kwargs=my_edge_labels_kwargs,                  patch_kwargs=my_patch_kwargs,                  legend_kwargs=my_legend_kwargs,                  utility_label_kwargs=my_utility_label_kwargs,                  utility_label_shift=0.1,                  info_sets_kwargs=my_info_sets_kwargs)     # s, b = subgame_perfect_equilibrium(game, uniform_strategy)  # print(s)  # print(b)    nfg = build_subgame(game, 4)    unif_strat = uniform_strategy(nfg)  # print(unif_strat)    f = nfg.incentive_target_function(unif_strat)  