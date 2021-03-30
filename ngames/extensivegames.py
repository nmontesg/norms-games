#!/usr/bin/env python3
# -*- coding: utf-8 -*-
r"""Implementation of games in extensive form.

The most important class of the module is ``ExtensiveFormGame'', which 
provides support for n-player extensive form games, including chance moves.
It also provides support for a graphical representation of the game tree and
implementation for the backward induction algorithm, which is used to compute
subgame-perfect equilibrium strategies and equilibrium paths that are expected
to be played by perfectly rational agents.

References
----------
J. González-Díaz, I. García-Jurado, M.G. Fiestras-Janeiro, An Introductory
Course on Mathematical Game Theory, American Mathematical Society and Real
Sociedad Matemática Española, 2010. https://doi.org/10.1016/j.geb.2010.12.006.

"""

import networkx as nx
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import pandas as pd
import random
from itertools import combinations
from copy import deepcopy
from typing import Any, Dict, List, Set, Tuple

__author__ = "Nieves Montes"
__copyright__ = "Copyright 2020, Nieves Montes"
__credits__ = ["Nieves Montes"]
__license__ = "MIT"
__version__ = "0.0.1"
__maintainer__ = "Nieves Montes"
__email__ = "nmontes@iiia.csic.es"
__status__ = "Development"

class ExtensiveFormGame:
  r"""Implementation of a game in extensive form.
  
  The game is initialized 'empty', meaning with minimal attribute assignments.
  Attributes are then set through the various methods. The extensive form game
  is modelled as described in the reference, see the chapter on extensive
  games.

  Parameters
  ----------
  **kwargs
    Additional keyword arguments.

  Attributes
  ----------
  game_tree : networkx.DiGraph
    Game tree, directed graph. Other than the methods and attributes of the
    class, two additional attributes are set:
      * root : Any
          The root node, initialized to None.
      * terminal_nodes : List[Any]
          The list of terminal nodes, initialized to an empty list.
    The game tree is initialized as empty.
  information_partition : Dict[Any, List[Set[Any]]]
    For every player (key), it maps it to the list of the information sets
    (values).
  is_perfect_informtion : bool, `True`
    The game is initialized as being of perfect information.
  players : List[Any]
    List of players in the game. It is initialized empty.
  probability : Dict[Any, Dict[Tuple[Any, Any], float]]
    Probability distributions over the outgoing edges at every node where 
    chance takes an action. The keys are the nodes where chance acts. The 
    values are dictionaries mapping every outgoing edge from that node to its
    probability.
  turn_function : Dict[Any, Any]
    Function that maps every non-terminal node to the player whose turn it 
    is to take an action at the node.
  utility : Dict[Any, Dict[Any, float]]
    For every terminal node, it maps the utility that the various players 
    (excluding chance) assign to it.
  
  See Also
  --------
  networkx.DiGraph
  
  """
  
  def __init__(self, **kwargs) -> None:    
    # players
    self.players = []
    
    # game tree
    self.game_tree = nx.DiGraph()
    self.game_tree.root = None
    self.game_tree.terminal_nodes = []
    
    # turn function
    self.turn_function = {}
    
    # information partition
    self.information_partition = {}
    self.is_perfect_information = True
    
    # probability distribution over chance edges
    self.probability = {}
    
    # utility function
    self.utility = {}
    
    # additional info
    for k, v in kwargs.items():
      setattr(self, k, v)
    
  def __str__(self) -> str:
    r"""Print the title of the game, if it has any.
    
    If not, just print 'Extensive Form Game'.
    
    Returns
    -------
    str
      The output to console when `print()` is called.

    """
    if 'title' in self.__dict__.keys():
      return "Extensive form game: {}".format(self.name)
    return "Extensive form game"
  
  def __check_player_in_game(self, player_id: Any) -> None:
    r"""Check that the given player is actually in the game.

    Parameters
    ----------
    player_id : Any

    Raises
    ------
    ValueError
      If the player is not in the game.

    """
    if player_id not in self.players:
      raise ValueError("player {} not in game".format(player_id))
        
  def __check_nonterminal_node(self, node_id: Any) -> None:
    r"""Check that a node is in the game tree.
    
    Parameters
    ----------
    node_id : Any

    Raises
    ------
    ValueError
      If the node is not in the game tree.

    """
    if node_id not in self.get_nonterminal_nodes():
      raise ValueError("node {} is a terminal node".format(node_id))
       
  def __check_terminal_node(self, node_id: Any) -> None:
    r"""Check that a node is terminal.

    Parameters
    ----------
    node_id : Any

    Raises
    ------
    ValueError
      If the node is not terminal.

    """
    if node_id not in self.game_tree.terminal_nodes:
      raise ValueError("node {} is not a terminal node".format(node_id))
  
  def add_players(self, *players_id: Any) -> None:
    r"""Add a lists of players to the game, encoded in any data structure.

    Parameters
    ----------
    players_id : List[Any]
      Players to be added to the game. Exclude 'chance'.
      
    Raises
    ------
    ValueError
      If 'chance' is among the players to be added.

    """
    for p in players_id:
      if p == 'chance':
        raise ValueError("player 'chance' should not added to the game")
      self.players.append(p)
      self.information_partition[p] = []
    
  def add_node(self, node_id: Any, player_turn: Any=None,
               is_root: bool=False) -> None:
    r"""Add a node the game tree.
    
    If the node is non-terminal and it is not a chance node, perfect 
    information is assumed. A set containing the single node is added to the
    information partition of the player playing at the node.
    
    Also, if the node is non-terminal (regardless of whether it is a chance
    node or not), it is added to `turn_function` and its player is assigned.    
    
    Parameters
    ----------
    node_id : Any
      Node to be added.
    player_turn : Any, optional
      Whose player has the turn at the node. If None is given, it is assumed
      that the node is terminal. The default is None.
    is_root : bool, optional
      Whether the node is the root of the game tree. The default is False.

    """    
    self.game_tree.add_node(node_id)
    
    # if player turn given
    if player_turn:
      self.turn_function[node_id] = player_turn
      # add player to game if not already there
      if player_turn not in self.players and player_turn != 'chance':
        self.players.append(player_turn)
      # if not a chance node, assume perfect information
      if player_turn != 'chance':
        self.__check_player_in_game(player_turn)
        self.information_partition[player_turn].append({node_id})

    # if player turn not given, it is a terminal node
    else:
      self.game_tree.terminal_nodes.append(node_id)

    # assign as root if indicated
    if is_root:
      self.game_tree.root= node_id
      
  def set_node_player(self, node_id: Any, player_turn: Any) -> None:
    r"""Set the player at a node after it has been added to the game tree.
    
    If the node had been designated as a terminal, remove it from that list.

    Parameters
    ----------
    node_id : Any
      The node whose player changes.
    player_turn : Any
      The new player that takes turn at the node.

    """
    self.turn_function[node_id] = player_turn
    # add player to game if not already there
    if player_turn not in self.players and player_turn != 'chance':
      self.players.append(player_turn)
    # delete node from terminal nodes if there
    if node_id in self.game_tree.terminal_nodes:
      self.game_tree.terminal_nodes.remove(node_id)
    
  def add_edge(self, from_node: Any, to_node: Any, label: Any) -> None:
    r"""Add an edge to the game tree between two nodes.

    Parameters
    ----------
    from_node : Any
      Origin node of the edge.
    to_node : Any
      Destination node of the edge.
    label : Any
      The edge label corresponsing to the action being take.

    """
    self.game_tree.add_edge(from_node, to_node, action=label)
    
  def get_nonterminal_nodes(self) -> List[Any]:
    r"""Obtain the list of non-terminal nodes in the game tree.

    Returns
    -------
    List[Any]
      List of non-terminal nodes.

    """
    nonterminal_nodes = []
    for n in self.game_tree.nodes:
      if n not in self.game_tree.terminal_nodes:
        nonterminal_nodes.append(n)
    return nonterminal_nodes
     
  def get_theta_partition(self) -> Dict[Any, Set[Any]]:
    r"""Get the turns partition.
    
    The turns partition (or :math:`\Theta` partition) splits the non-terminal
    nodes into disjunct sets, according to whose turn it is to play at the
    node (including the 'chance' player).    

    Returns
    -------
    Dict[Any, Set[Any]]
      For every player in the game, including 'chance', the set of nodes 
      where it is that player's turn to play.

    """
    # initialize partitions to empty set
    theta_partition = {}
    for p in self.players:
      theta_partition[p] = set()
    theta_partition['chance'] = set()
    # add nodes to their corresponding partition
    for n in self.get_nonterminal_nodes():
      node_turn = self.turn_function[n]
      theta_partition[node_turn].add(n)
    return theta_partition
  
  def get_player_utility(self, player_id: Any) -> Dict[Any, float]:
    r"""Return the utility function for the given player.

    Parameters
    ----------
    player_id : Any

    Returns
    -------
    Dict[Any, float]
      A map from every terminal node to the utility assigned to it by the
      given player.

    """
    self.__check_player_in_game(player_id)
    utility_i = {}
    for n in self.game_tree.terminal_nodes:
      utility_i[n] = self.utility[n][player_id]
    return utility_i
  
  def get_available_actions(self, node: Any) -> Set[Any]:
    r"""Get what actions are available at the given node.

    Parameters
    ----------
    node : Any

    Returns
    -------
    Set[Any]
      Set of available actions according to the game tree.

    """
    actions = set()
    for e in self.game_tree.out_edges(node):
      a = self.game_tree.get_edge_data(*e)['action']
      actions.add(a)
    return actions

  def get_choice_set(self, player_id: Any, information_set: Set[Any]) \
    -> Set[Any]:
    r"""Get the choice set for some player at some information set.
    
    Parameters
    ----------
    player_id : Any
    information_set : Set[Any]
      The information set for which the choice set is to be retrieved.

    Returns
    -------
    List[Tuple[Any]]
      List of edges outgoing from every node in the information set.

    """
    self.__check_player_in_game(player_id)
    assert information_set in self.information_partition[player_id], \
      "information set {} does not belong to player {}'s information \
        partition".format(information_set, player_id)
    choice_set = self.get_available_actions(list(information_set)[0])
    return choice_set
  
  def get_utility_table(self) -> pd.DataFrame:
    r"""Get a pandas dataframe with the utility for every player.

    Returns
    -------
    utility_table : pandas.DataFrame

    """
    data = {}
    terminal_nodes = self.game_tree.terminal_nodes
    data['Terminal node'] = terminal_nodes
    for pos in self.players:
      data[pos.capitalize()] = [self.utility[n][pos] for n in
                                             terminal_nodes]
    utility_table = pd.DataFrame(data)
    utility_table.set_index('Terminal node', inplace=True)
    return utility_table
  
  def add_information_sets(self, player_id: Any,
                                *additional_info_sets: Set[Any]) -> None:
    r"""Add an information set to the partition of the given player.
    
    This method does not require that all nodes where ``player_id`` takes
    an actions are included in some information set. It does check that all
    the nodes in the information partition to be added belong to the theta
    partition of ``player_id``, and that they have no been previously
    included in some other information set.

    Parameters
    ----------
    player_id : Any
      The game player whose information partition is to be expanded.
    *additional_info_sets : Set[Any]
      The information sets that are to be added.

    """
    self.__check_player_in_game(player_id)
    
    self.is_perfect_information = False
    
    # check that the nodes in the information sets belong to the theta
    # partition of the player
    theta_partition = self.get_theta_partition()[player_id]
    # check that the nodes in the additional information sets are not already
    # in the information partition
    all_sets = self.information_partition[player_id]
    info_sets_union = [x for y in all_sets for x in y]
    for s in additional_info_sets:
      for n in s:
        assert n in theta_partition, "node {} not in the turn function of \
          player {}".format(n, player_id)
        assert n not in info_sets_union, "node {} already in information \
          partition of player {}".format(n, player_id)
    
    for s in additional_info_sets:
      self.information_partition[player_id].append(s)
    
  def set_information_partition(self, player_id: Any,
                                *partition: Set[Any]) -> None:
    r"""Set the information partition of the given player.
    
    It is only useful to call this method when modeling games with imperfect
    information, otherwise when nodes are added to the game tree perfect
    information is assumed by default.
    
    The method checks that all the nodes where it is the player's turn to 
    move are included in the information partition, and viceversa, that at all 
    the nodes in the various information sets it is the player's turn. Also,
    it checks that all the nodes in any given information set have the same 
    number of outgoing edges, and that they are non-terminal.

    Parameters
    ----------
    player_id : Any
    
    partition : Set[Any]
      Information sets making up the player's information
      partition.

    Raises
    ------
    AssertionError
      If the union of information sets does not correspon to the same nodes
      where it is the player's turn to play, or
      If some nodes in the same information set have different amounts of
      outgoing edges, or
      If some node is terminal.      
    
    Notes
    -----
    Please note that the method does not check that all the information sets
    provided are disjunct.    

    """
    self.__check_player_in_game(player_id)
    
    self.is_perfect_information = False
    
    # check that all the nodes where the player plays are included in the 
    # information partition
    theta_player = self.get_theta_partition()[player_id]
    nodes_in_info_sets = set()
    for p in partition:
      nodes_in_info_sets.update(p)
    assert theta_player == nodes_in_info_sets, "the information set for\
      player {} is missing some nodes".format(player_id)    
    
    for p in partition:
      # check that all nodes in information set have the same available 
      # actions
      all_avail_actions = [self.get_available_actions(n) for n in p]
      assert all(av == all_avail_actions[0] for av in all_avail_actions), \
        "nodes in information set {} have different available actions"\
          .format(p)
      
      # check that nodes are not terminal
      for n in p:
        self.__check_nonterminal_node(n)
    
    # replace current partition with the new one
    self.information_partition[player_id] = []
    for p in partition:
      self.information_partition[player_id].append(p)
  
  def set_probability_distribution(self, node_id: Any,
    prob_dist: Dict[Tuple[Any], float]) -> None:
    r"""Set the probabilities over the outgoing edges of a chance node.

    Parameters
    ----------
    node_id : Any
      Node over whose outgoing edges the probability is given.
    prob_dist : Dict[Tuple[Any], float]
      Probability distribution over the outgoing edges of the node.

    Raises
    ------
    ValueError
      If at the given node, it is not chance's turn, or
      If one of the provided edges does not have the given node as origin, or
      If there is some edge going out from the node for which the probability
      is not specified
    AssertionError
      If the sum of the probabilities over all the edges is not close to unity
      with :math:`10-^{3}` absolute tolerance.      

    """
    if self.turn_function[node_id] != 'chance':
      raise ValueError("it is not chance's turn at node {}".format(node_id))
    outgoing_edges = self.game_tree.out_edges(node_id)
    for e in prob_dist.keys():
      if e not in outgoing_edges:
        raise ValueError("edge {} is not an outgoing edge from {}"\
                         .format(e, node_id))
    for e in outgoing_edges:
      if e not in prob_dist.keys():
        raise ValueError("probability not specified for edge {}".format(e))
    assert np.isclose([sum(prob_dist.values())], [1], atol=1.E-3)[0], "sum \
      over probability distribution of edges must be close to 1"
    self.probability[node_id] = prob_dist
      
  def set_uniform_probability_distribution(self, node_id: Any) -> None:
    r"""Set a equal probabilities over the outgoing edges of a chance node.

    Parameters
    ----------
    node_id : Any
      A node where chance takes its turn.

    """
    outgoing_edges = self.game_tree.out_edges(node_id)
    uniform_prob_dist = {e:1/len(outgoing_edges) for e in outgoing_edges}
    self.set_probability_distribution(node_id, uniform_prob_dist)
     
  def set_utility(self, node_id: Any, utilities: Dict[Any, float]) -> None:
    r"""Set the utility for all players at the given terminal node.

    Parameters
    ----------
    node_id : Any
      A terminal node.
    utilities : Dict[Any, float]
      Dictionary that maps every player in the game to the utility it
      assigns to the terminal node.

    """
    self.__check_terminal_node(node_id)
    self.utility[node_id] = {}
    for pos, u in utilities.items():
      self.__check_player_in_game(pos)
      self.utility[node_id][pos] = u
   

def hierarchy_pos(G: Any, root: Any=None, width: float=1.,
                  vert_gap: float=0.2, vert_loc: float=0,
                  xcenter: float=0.5) -> Dict[Any, Tuple[float, float]]:
    r"""From Joel's answer at https://stackoverflow.com/a/29597209/2966723.
    
    Licensed under Creative Commons Attribution-Share Alike.
    
    If the graph is a tree this will return the players to plot this in a 
    hierarchical layout.
    
    Parameters
    ----------
    G : Any
      The graph (must be a tree). In practive, must be an instance of one of
      the classes provided by `networkx`.
    root : Any, optional
      The root node of current branch. The default is None.
      * If the tree is directed and this is not given, the root will be found
      and used.
      * If the tree is directed and this is given, then  the players will be 
      just for the descendants of this node.
      * If the tree is undirected and not given, then a random choice will be 
      used.
    width : float, optional
      Horizontal space allocated for this branch - avoids overlap with other 
      branches. The default is 1..
    vert_gap : TYPE, optional
      Gap between levels of hierarchy. The default is 0.2.
    vert_loc : TYPE, optional
      Vertical location of root. The default is 0.
    xcenter : TYPE, optional
      Horizontal location of root. The default is 0.5.
  
    Raises
    ------
    TypeError
      If the graph is not a tree.
  
    Returns
    -------
    Dict[Any, Tuple[float, float]]
      Mapping from every node in the tree to its layout player.
      
    See Also
    --------
    networkx.is_tree

    """
    if not nx.is_tree(G):
      raise TypeError('cannot use hierarchy_pos on a graph that is not a \
                      tree')
    if root is None:
      if isinstance(G, nx.DiGraph):
        root = next(iter(nx.topological_sort(G)))
      else:
        root = random.choice(list(G.nodes))

    def _hierarchy_pos(G: Any, root: Any=None, width: float=1.,
                       vert_gap: float=0.2, vert_loc: float=0, 
                       xcenter: float=0.5,
                       pos: Dict[Any, Tuple[float, float]]=None,
                       parent: Any=None):
        r"""See hierarchy_pos for most arguments.
        
        Parameters
        ----------
        pos : Dict[Any, Tuple[float, float]]
          A dictionary saying where all nodes go if they have been assigned.
          Default is None.
        parent : Any
          Parent of this branch - only affects it if non-directed.
          Default is None.

        """
        if pos is None:
          pos = {root:(xcenter,vert_loc)}
        else:
          pos[root] = (xcenter, vert_loc)
        children = list(G.neighbors(root))
        if not isinstance(G, nx.DiGraph) and parent is not None:
          children.remove(parent)  
        if len(children) != 0:
          dx = width/len(children)
          nextx = xcenter - width/2 - dx/2
          for child in children:
            nextx += dx
            pos = _hierarchy_pos(G, child, width=dx, vert_gap=vert_gap,
                                 vert_loc=vert_loc-vert_gap, xcenter=nextx,
                                 pos=pos, parent=root)
        return pos
      
    return _hierarchy_pos(G, root, width, vert_gap, vert_loc, xcenter)
    

def plot_game(game: ExtensiveFormGame, player_colors: Dict[Any, str],
              utility_label_shift: float=0.03,
              fig_kwargs: Dict[str, Any]=None,
              node_kwargs: Dict[str, Any]=None,
              edge_kwargs: Dict[str, Any]=None,
              edge_labels_kwargs: Dict[str, Any]=None,
              patch_kwargs: Dict[str, Any]=None,
              legend_kwargs: Dict[str, Any]=None,
              draw_utility: bool=True,
              utility_label_kwargs: Dict[str, Any]=None,
              info_sets_kwargs: Dict[str, Any]=None) -> plt.Figure:
  r"""Make a figure of the game tree.
  
  Encoded information:
    * Node colors encode the turn function at every node.
    * Dashed archs between nodes indicate information sets.
    * Numbers in parenthesis below terminal nodes indicate utilities
    (optional).

  Parameters
  ----------
  game : ExtensiveFormGame
    A game in extensive form to be plotted.
  player_colors : Dict[Any, str]
    Dictionary mapping every player in the game to the color to use for the
    nodes where it is the player's turn. Color white is not recommended, as 
    it is reserved for chance nodes.
  utility_label_shift : float, optional
    To adjust the utility labels under the terminal nodes. The default is 0.03.
  fig_kwargs : Dict[str, Any], optional
    Additional keywork arguments related to the rendering of the figure - they
    are passed to `matplotlib.pyplot.subplots`. The default is None.
  node_kwargs : Dict[str, Any], optional
    Additional keyword arguments related to the rendering of the game tree 
    nodes - they are passed to `nx.draw_network`. The default is None.
  edge_kwargs : Dict[str, Any], optional
    Additional keyword arguments related to the rendering of the game tree 
    edges - they are passed to `nx.draw_network`. The default is None.
  edge_labels_kwargs : Dict[str, Any], optional
    Additional keyword arguments related to the rendering of the edge labels
    - they are passed to `nx.draw_network_edge_labels`. The default is None.
  patch_kwargs : Dict[str, Any], optional
    Additional keyword arguments related to the rendering of the legend 
    patches - they are passed to `matplotlib.patches.Patch`. The default is 
    None.
  legend_kwargs : Dict[str, Any], optional
    Additional keyword arguments related to the rendering of the legend - they 
    are passed to `matplotlib.axes.Axes.legend`. The default is None.
  draw_utility : bool, optioanl
    Whether labels should be drawn below the terminal nodes displaying the 
    utilities for all players. The default is True.
  utility_label_kwargs : Dict[str, Any], optional
    Additional keyword arguments related to the rendering of the utility 
    labels at the terminal nodes - they are passed to 
    `matplotlib.pyplot.text`. The default is None.
  info_sets_kwargs : Dict[str, Any], optional
    Additional keyword arguments related to the rendering of the archs 
    connecting the information sets - they are passed to 
    `matplotlib.patches.Arch`. The default is None.

  Returns
  -------
  fig : matplotlib.figure.Figure

  """
  pos = hierarchy_pos(game.game_tree, game.game_tree.root)
  fig, ax = plt.subplots(**fig_kwargs)
  fig.patch.set_visible(False)
  
  # if there is chance in the game and it does not have a color, set it to
  # white
  if game.get_theta_partition()['chance'] != set():
    if 'chance' not in player_colors.keys():
      player_colors['chance'] = 'white'
  
  # draw the game tree
  node_col = []
  for n in game.game_tree.nodes:
    if n in game.game_tree.terminal_nodes:
      col = 'silver'
    else:
      player = game.turn_function[n]
      col = player_colors[player]
    node_col.append(col)
  nx.draw_networkx(game.game_tree, pos=pos, ax=ax, with_labels=True,
                   node_color=node_col, **node_kwargs, **edge_kwargs)
  
  # prepare edge labels
  edge_labels = {}
  for e in game.game_tree.edges:
    label = game.game_tree.get_edge_data(*e)['action']
    parent_node = e[0]
    parent_player = game.turn_function[parent_node]
    # if edge is action from chance, add probability
    if parent_player == 'chance':
      prob = game.probability[parent_node][e]
      label += ' ({:.2f})'.format(prob)
    edge_labels[e] = label
  # draw edge labels
  nx.draw_networkx_edge_labels(game.game_tree, pos=pos, ax=ax,
                               edge_labels=edge_labels, **edge_labels_kwargs)
  
  # draw legend
  handles = []
  for player, col in player_colors.items():
    patch = mpatches.Patch(color=col, label=player, **patch_kwargs)
    patch.set_edgecolor('black')
    handles.append(patch)
  ax.legend(handles=handles, **legend_kwargs)
  
  # draw utility on terminal nodes
  if draw_utility:
    terminal_nodes = game.game_tree.terminal_nodes
    for n in terminal_nodes:
      utility_label_player = (pos[n][0], pos[n][1]-utility_label_shift)
      utilities_node = ["{:.1f}".format(game.utility[n][p]) \
                        for p in game.players if p!='chance']
      utility_label = '{}'.format('\n'.join(utilities_node))
      plt.text(*utility_label_player, utility_label, **utility_label_kwargs)
    
  # draw archs between nodes in the same information set
  for player in game.players:
    if player == 'chance':
      continue
    for info_set in game.information_partition[player]:
      if len(info_set) == 1:
        continue
      for u, v in combinations(info_set, r=2):
        x = (pos[u][0] + pos[v][0])/2
        y = (pos[u][1] + pos[v][1])/2
        width = abs(pos[u][0] - pos[v][0])
        height = 0.1
        arch = mpatches.Arc((x, y), width, height, theta1=0, theta2=180,
                            edgecolor=player_colors[player], fill=False,
                            **info_sets_kwargs)
        ax.add_patch(arch)
  
  plt.show()
  plt.close(fig)
  return fig


def backward_induction(game: ExtensiveFormGame, h: Any, 
                       u_dict: Dict[Any, Dict[Any, float]]={}) \
  -> Dict[Any, Dict[Any, float]]:
  r"""Compute the value of node `h` in a subgame by backward induction.
  
  It computes the values of all the nodes in the subgame having `h` as its
  root node. Only for games with perfect information.

  Parameters
  ----------
  game : ExtensiveFormGame
    A game in extensive forms. Must be a perfect information game without any
    stochastic effects (no `chance` nodes).
  h : Any
    The root of the subgame where to start computing.
  u_dict : Dict[Any, Dict[Any, float]], optional
    A dictionary of the values for every player at the nodes that have
    already been revisited. The default is {}, and it should not be modified.
    It is necessary to perform the recursion.

  Returns
  -------
  Dict[Any, Dict[Any, float]]
    A dictionary mapping, for each visited node (all the descendants of `h`),
    the value assigned to it by every player in the game.

  """
  if h in game.game_tree.terminal_nodes:
    u_dict[h] = game.utility[h]
    return u_dict
  player = game.turn_function[h]
  if player == 'chance':
    u = {p:0. for p in game.players}
  else:
    u = {p:-float('inf') for p in game.players}
  u_dict[h] = u
  for e in game.game_tree.out_edges(h):
    child = e[1]
    u_child = backward_induction(game, child, u_dict)[child]
    if player == 'chance':
      prob_edge = game.probability[h][e]
      for pos in game.players:
        u[pos] += prob_edge*u_child[pos]
    else:
      if u_child[player] > u[player]:
        u = u_child
  u_dict[h] = u
  return u_dict


def subgame_perfect_equilibrium(game: ExtensiveFormGame) -> Dict[Any, Any]:
  r"""Find a subgame perfect equilibrium in pure strategies.
  
  Parameters
  ----------
  game : ExtensiveFormGame
    The game in extensive form.

  Returns
  -------
  SPE : Dict[Any, Any]
    A subgame perfect equilibrium in pure strategies, mapping each nodes to
    the action to be taken.

  """
  # assert game.is_perfect_information, "subgame perfect equilibrium is only \
  #   applicable to games with perfect information"
  # get the values of the nodes in the subgame by backward induction
  values_dict = backward_induction(game, game.game_tree.root)
  SPE = {}
  for n in game.game_tree.nodes:
    if n in game.game_tree.terminal_nodes:
      continue
    player = game.turn_function[n]
    if player == 'chance':
      continue
    next_value = -float('inf')
    action = None
    for e in game.game_tree.out_edges(n):
      child = e[1]
      if values_dict[child][player] > next_value:
        next_value = values_dict[child][player]
        action = (e, game.game_tree.get_edge_data(*e)['action'])
    SPE[n] = action
  return SPE

 
def DFS_equilibria_paths(game: ExtensiveFormGame, h: Any, 
                         pure_strategy: Dict[Any, Any], path: List[Any], 
                         probability: float, 
                         path_store: List[Tuple[List[Any], float]]) -> None:
  r"""Find all the equilibrium paths.
  
  This function finds all of the paths given the deterministic strategy and
  considering the chance nodes, and stores (path, probability) tuples in a 
  `store`.

  Parameters
  ----------
  game : ExtensiveFormGame
    The game being played.
  h : Any
    The node where play starts.
  pure_strategy : Dict[Any, Any]
    A dictionary mapping every decision node to the (edge, action) pair to be
    followed.
  path : List[Any]
    The path played before reaching the current node.
  probability : float
    The probability of playing the path played before reaching the current 
    node.
  path_store : List[Tuple[List[Any], float]]
    A store where the computed paths are stores alongside with their 
    probabilities of being played.
    
  Examples
  --------
  The intended way to call this function is:
  >>> path_store = []
  >>> DFS_equilibria_paths(game, game.game_tree.root, pure_strat, [], 1, \
                           path_store)
  >>> print(path_store)

  """
  path.append(h)
  # if the current node is a decision node
  if h in pure_strategy.keys():
    next_node = pure_strategy[h][0][1]
    action = pure_strategy[h][1]
    path.append(action)
    DFS_equilibria_paths(game, next_node, pure_strategy, path, probability,
                         path_store)
  # if the current node is a chance node
  elif h in game.turn_function.keys() and game.turn_function[h] == 'chance':
    prob_until_chance = probability
    for e in game.game_tree.out_edges(h):
      path_until_chance = deepcopy(path)
      next_node = e[1]
      action = game.game_tree.get_edge_data(*e)['action']
      path_until_chance.append(action)
      prob = prob_until_chance*game.probability[h][e]
      DFS_equilibria_paths(game, next_node, pure_strategy, path_until_chance,
                           prob, path_store)
  # if current node is terminal, append path to the store
  else:
    path_store.append((path, probability))
    
    
if __name__ == '__main__':
  pass
