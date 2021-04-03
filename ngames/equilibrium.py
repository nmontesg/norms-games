#!/usr/bin/env python3# -*- coding: utf-8 -*-r"""Compute equilibrium paths of play using backward induction.Assume sub-game perfect rationality.To be applied on extensive-form games that are equivalent to heterogenousnormal form games played sequentially, i.e. games that have been generatedfrom a set of Prolog files and the ``build.build_full_game`` function."""import numpy as npfrom networkx.algorithms.dag import descendantsfrom itertools import productfrom copy import deepcopyfrom scipy.optimize import Bounds, minimizefrom extensivegames import ExtensiveFormGamefrom normalgames import NormalFormGamefrom typing import Callable, Dict, Any, Tuple, List__author__ = "Nieves Montes"__copyright__ = "Copyright 2020, Nieves Montes"__credits__ = ["Nieves Montes"]__license__ = "MIT"__version__ = "0.0.1"__maintainer__ = "Nieves Montes"__email__ = "nmontes@iiia.csic.es"__status__ = "Prototype"def build_subgame(extensive_game: ExtensiveFormGame, root: int) \  -> NormalFormGame:  r"""Build a normal form game that emanates from a node in an extensive game.    Assumes that the root from which the normal form game is built corresponds  to the root of the last round of the extensive game.  Parameters  ----------  extensive_game : ExtensiveFormGame  root : int    The root from which a last rounds of the extensive game starts.  Raises  ------  AssertionError    If a descendant of a chance node is not terminal.  ValueError    If a path of play does not end at a chance or at a terminal node.  Returns  -------  normal_form_game : NormalFormGame    The game in normal form corresponding to the .  """  # get the players and their possible actions  player_actions = {}  n = root  while True:    try:      turn = extensive_game.turn_function[n]      if turn == 'chance':        break    # KeyError happens if terminal node is reached, not in the turn function    except KeyError:      break      active_player = extensive_game.turn_function[n]    out_edges = list(extensive_game.game_tree.out_edges(n, data='action'))    actions = tuple(o[2] for o in out_edges)    player_actions[active_player] = actions    n = out_edges[0][1]      players = extensive_game.players  actions = []  for p in players:    try:      actions.append(player_actions[p])    except KeyError:      actions.append(('no-op',))  # build the payoff function  payoff_function = {}  non_empty_actions = [a for a in actions if a]  possible_play_paths = product(*non_empty_actions)    # scan through the possible paths of play  for p in possible_play_paths:    n = root    for action in p:      if action == 'no-op':        continue      out_edges = list(extensive_game.game_tree.out_edges(n, data='action'))      next_node = [o[1] for o in out_edges if o[2] == action][0]      n = next_node          # last node of play path is a terminal node    if n in extensive_game.game_tree.terminal_nodes:      utility = tuple(extensive_game.utility[n][p] for p in players)    # last node of play path is a chance node: weight average over    # descendant terminal nodes    elif extensive_game.turn_function[n] == 'chance':      utility_dict = {pl: 0 for pl in players}      for (_, desc), prob in extensive_game.probability[n].items():        assert desc in extensive_game.game_tree.terminal_nodes, "node {} \          should be a terminal node".format(desc)        for pl in players:          utility_dict[pl] += prob*extensive_game.utility[desc][pl]      utility = tuple(utility_dict[pl] for pl in players)    else:      raise ValueError("node at end of path play {} from root node {} is not \                       a terminal nor a chance node".format(p, root))        payoff_function[p] = utility    normal_form_game = NormalFormGame(    players=players,    actions=actions,    payoff_function=payoff_function    )    return normal_form_gamedef uniform_strategy(normal_game: NormalFormGame) \  -> Dict[Any, Dict[Any, float]]:  r"""Return the perfectly mixed strategy over a normal form game.    This is not a realistic equilibrium strategy computation, just for debugging  purposes.    Parameters  ----------  normal_game : NormalFormGame  Returns  -------  Dict[Any, Dict[Any, float]]    The mixed strategy that randomizes over all available actions for all    players.  """  strategy = {}  for n, p in enumerate(normal_game.players):    num_actions = len(normal_game.player_actions[n])         strategy[p] = {a: 1/num_actions for a in normal_game.player_actions[n]}  return strategy  def scalar_function(x: np.array) -> float:  r"""Function to be minimized: total deviation incentives for all players.    Written in a format such that the only input is a mixed strategy encoded  as a numpy.array.  Parameters  ----------  x : numpy.array    Array encoding a mixed strategy. The game to which it is to be applied is    set as an attribute of the function externally  Raises  ------  AttributeError    If, at the time of being called, no normal-form game has been set as an    attribute, to which the mixed strategy array encoded in the array is     passed to compute the incentives to deviate.  Returns  -------  f : float    The function to be minimixed, i.e. the total incentive to deviate from    the mixed strategy:          .. math::            f(s) = \sum\limits_{i \in G} \sum\limits_{j \in A_j} (d_i^j(s))^2        References  ----------  Shohan, Y., & Leyton-Browm, K. (2009). Computing Solution Concepts of  Normal-Form Games. In Multiagent Systems: Algorithmic, Game-Theoretic,  and Logical Foundations (pp. 87–112). Cambridge University Press.  """  if not scalar_function.game:    raise AttributeError("scalar_function() called without game attribute")  # build mixed strategy as dictionary from array  k = 0  mixed_strat = {}  for n in range(scalar_function.game.num_players):    player = scalar_function.game.players[n]    player_strat = {}    for a in scalar_function.game.player_actions[n]:      player_strat[a] = x[k]      k += 1    mixed_strat[player] = player_strat  f = scalar_function.game.incentive_target_function(mixed_strat)  return f  def minimize_incentives(normal_game: NormalFormGame) \  -> Dict[Any, Dict[Any, float]]:  r"""Compute the mixed strategy that minimizes the incentive to deviate.    Given an arbitrary normal-form game, compute the (in general, mixed)  strategy profile that minimizes the incentives to deviate from all players.  The target function being minimized is:      .. math::            f(s) = \sum\limits_{i \in G} \sum\limits_{j \in A_j} (d_i^j(s))^2 \\      c_{i}^{j}(s) = u_i(a_{j}^{i}, s_{-j}) - u_i(s) \\      d_{i}^{j}(s) = max(c_{i}^{j}(s), 0)        It uses the scipy.optimize library to solve the optimization problem. In  particular, their implementation of the Sequential Least Squares Programming  (SLSQP) algorithm.  Parameters  ----------  normal_game : NormalFormGame      Raises  ------  ValueError    If the optimization (i.e. the call to ``scipy.optimize.minimize``) is not    successfull.  Returns  -------  Dict[Any, Dict[Any, float]]    The mixed strategy that minimizes the total incentives to deviate, as a     dictionary mapping every player in the game to a mixed strategy.  """  # dimension of the search space  player_num_actions = [len(act) for act in                        normal_game.player_actions.values()]    counter = 0  player_action_indices = []  for n in range(normal_game.num_players):    player_action_indices.append((counter, counter+player_num_actions[n]-1))    counter += player_num_actions[n]    # bounds: all between 0 and 1  vector_length = sum(player_num_actions)  bounds = Bounds([0]*vector_length, [1]*vector_length)    # equality constraints  equality_constraint_gradient = []  for i, j in player_action_indices:    gradient = [0 for _ in range(vector_length)]    for k in range(i,j+1):      gradient[k] = 1    equality_constraint_gradient.append(gradient)    eq_constraint = {'type': 'eq',                   'fun': lambda x: np.array([x[i:j+1].sum() - 1 \                                    for i,j in player_action_indices]),                   'jac': lambda x: np.array(equality_constraint_gradient)  }    # initial guess  x0_list = []  for p in player_num_actions:    x0_player = [np.random.randint(10) for _ in range(p)]    total = sum(x0_player)    try:      x0_player = [x/total for x in x0_player]    except ZeroDivisionError:      x0_player = [1/p for _ in range(p)]    x0_list += x0_player  x0 = np.array(x0_list)    # set attribute of the function to minimize, since it can only take the  # vector array as argument  setattr(scalar_function, 'game', normal_game)      opt = minimize(scalar_function,                 x0,                 method='SLSQP',                 constraints=[eq_constraint],                 options={'ftol': 1.E-6},                 bounds=bounds)    delattr(scalar_function, 'game')    # check that the minimization has been successful  if not opt.success:    raise ValueError(opt.message)      # from vector to mixed strategy dictionary  mixed_strat_eq = {}  k = 0  for n in range(normal_game.num_players):    player = normal_game.players[n]    player_strat = {}    for a in normal_game.player_actions[n]:      player_strat[a] = opt.x[k]      k += 1    mixed_strat_eq[player] = player_strat    return mixed_strat_eqdef subgame_perfect_equilibrium(extensive_form_game: ExtensiveFormGame, \  equilibrium_function: Callable) -> Tuple[Dict[Any, Any], Dict[Any, Any]]:  r"""Compute the sequential equilibriums in an Extensive Form Game.    This function works on an Extensive Form Game built as a sequence of,  possibly different, Normal Form Games. This function identifies the subgames  starting from those closer to the end of the game (i.e. to the root nodes),  and passes them to ``build_subgame`` to build that part of the game tree as  a normal form game. Then, it calls the provided equilibrium function to  find the mixed strategy incentivized in that subgame, and backtracks the  resulting utility up the game tree.    Parameters  ----------  extensive_form_game : ExtensiveFormGame  equilibrium_function : Callable    The function that computes a solution concept on a (single-rounds) normal    form game. It should return the result as a dictionary with the game    players as keys, and a dictionary mapping their available actions to the    probabilities as values.  Returns  -------  subgame_mixed_strategies : Dict[Any, Any]    The mapping from the nodes that are the roots of the subgames to the mixed    equilibrium strategies computed for their subgame.  backtrack_utilities : Dict[Any, Any]    The mapping from the nodes that are the roots of the subgames to the    utilities resulting from the computed equilibrium strategies.  """  extensive_game = deepcopy(extensive_form_game)  max_rounds = max(extensive_game.node_rounds.values())  subgame_mixed_strategies = {}  backtrack_utilities = {}  subgame_rounds = max_rounds-1    while subgame_rounds >= 0:    subgame_root_nodes = [n for n in extensive_game.node_rounds.keys() \      if extensive_game.node_rounds[n] == subgame_rounds and      n not in extensive_game.game_tree.terminal_nodes]        # compute equilibrium at the subgames closest to the terminal nodes    for s in subgame_root_nodes:      # build the last subgame as an extensive form game      normal_game = build_subgame(extensive_game, s)            # function to compute the mixed strategy equilibrium here      mixed_equilibrium_strategy = equilibrium_function(normal_game)            subgame_mixed_strategies[s] = mixed_equilibrium_strategy            # store utility at the root node of the subgame      mes_list = [mixed_equilibrium_strategy[p] for p in normal_game.players]      rewards = normal_game.mixed_strategies_rewards(*mes_list)      backtrack_utilities[s] = {p: r for p, r in \                                zip(normal_game.players, rewards)}        for s in subgame_root_nodes:      # delete all descendants from subgame root nodes      desc = descendants(extensive_game.game_tree, s)      extensive_game.game_tree.remove_nodes_from(desc)            # remove previous terminal nodes from terminal list      terminal_previous = [d for d in desc \                           if d in extensive_game.game_tree.terminal_nodes]      for t in terminal_previous:        extensive_game.game_tree.terminal_nodes.remove(t)      extensive_game.game_tree.terminal_nodes.append(s)            # remove previous non-terminal nodes from turn function      for d in desc:        if d in extensive_game.turn_function.keys():          extensive_game.turn_function.pop(d)                # remove subgame root node from turn function because now it is terminal      extensive_game.turn_function.pop(s)            # backtrack utility      extensive_game.set_utility(s, backtrack_utilities[s])    subgame_rounds -= 1    # rounds mixed strategies to 3 decimal places  for n in subgame_mixed_strategies:    for p in extensive_game.players:      subgame_mixed_strategies[n][p] = {k: round(v, 3)         for k, v in subgame_mixed_strategies[n][p].items()}      return subgame_mixed_strategies, backtrack_utilitiesdef paths_of_play(extensive_game: ExtensiveFormGame,                  rounds_strat: Dict[Any, Any]) \  -> List[Tuple[List[Any], float]]:    pass    if __name__ == '__main__':  from numpy.random import randint  from extensivegames import ExtensiveFormGame, plot_game  from build import build_full_game    game = build_full_game('/home/nmontes/OneDrive/Documentos/PhD/ngames/examples/gamesFromRules/fishing-game', 'fishers', threshold=0)    for t in game.game_tree.terminal_nodes:    u = {p: randint(10) for p in game.players}    game.utility[t] = u    my_fig_kwargs = dict(figsize=(20,14), frameon=False)      my_fig_kwargs = dict(figsize=(20,10), frameon=False)  my_node_kwargs = dict(font_size=10, node_size=500, edgecolors='k',                        linewidths=1.5)  my_edge_kwargs = dict(arrowsize=15, width=1.5)  my_edge_labels_kwargs = dict(font_size=10)  my_patch_kwargs = dict(linewidth=2)  my_legend_kwargs = dict(fontsize=16, loc='upper right', edgecolor='white')  my_utility_label_kwargs = dict(horizontalalignment='center', fontsize=10)  my_info_sets_kwargs = dict(linestyle='--', linewidth=3)    position_colors =  {'alice':'aquamarine', 'bob':'greenyellow'}    fig = plot_game(game,                  position_colors,                  fig_kwargs=my_fig_kwargs,                  node_kwargs=my_node_kwargs,                  edge_kwargs=my_edge_kwargs,                  edge_labels_kwargs=my_edge_labels_kwargs,                  patch_kwargs=my_patch_kwargs,                  legend_kwargs=my_legend_kwargs,                  utility_label_kwargs=my_utility_label_kwargs,                  utility_label_shift=0.1,                  info_sets_kwargs=my_info_sets_kwargs)  s, b = subgame_perfect_equilibrium(game, minimize_incentives)  print(s)  print()  print(b)  print()  nfg = build_subgame(game, 4)    # unif_strat = uniform_strategy(nfg)  # print(unif_strat)    # f = nfg.incentive_target_function(unif_strat)  # print(f)    # res = equilibrium_strategy(nfg)  