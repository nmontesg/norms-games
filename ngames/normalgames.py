#!/usr/bin/env python3
# -*- coding: utf-8 -*-
r"""Implementation of  games in normal form.

This module provide the ``NormalFormGame'' class, which implements a normal-
form game (one-shot) with any number of players. It also provided the 
instances ``prisoners_dilemma'' and ``rock_paper_scissors'' as examples.

"""

import numpy as np
import pandas as pd
from itertools import product
from typing import List, Any, Dict, Tuple, Set

__author__ = "Nieves Montes"
__copyright__ = "Copyright 2020, Nieves Montes"
__credits__ = ["Nieves Montes"]
__license__ = "MIT"
__version__ = "0.0.1"
__maintainer__ = "Nieves Montes"
__email__ = "nmontes@iiia.csic.es"
__status__ = "Development"

class NormalFormGame:
  r"""A general, n-player general-sum normal-form game.
  
  A finite, n-person normal form game is a tuple :math:`(N, A, u)`, where:
    * :math:`N=\{1, 2, ..., n\}` is the set of players.
    * :math:`A=A_1\times A_2 \times ... \times A_n` is the set of joint
    actions, where :math:`A_i` is the set of actions available to player
    :math:`i`.
    * :math:`u=(u_1,u_2,...,u_n)`, where :math:`u_i:A\rightarrow R` are real-
    valued payoff functions.

  Parameters
  ----------
  players : List[Any]
    The list of players that participate in the game.
  actions : List[Tuple[Any, ...]]
    A list of tuples. Each tuple corresponds to the domain of actions (or
    'pure' strategies) available to each player in order, *i.e.* actions for 
    player 1, actions for player 2, etc.
  payoff_function : Dict[Tuple[Any, ...], Tuple[float, ...]]
    A dictionary mapping joint actions to player's payoffs. The dictionary 
    format is: tuple of joint actions (key) : tuple of players' float value 
    payoffs (value).
  **kwargs
    Arbitrary keyword arguments.
    
  Attributes
  ----------
  action_to_index : Dict[int, Dict[Any, int]]
    A dictionary of dictionaries. For every player, it stores a dictionary
    mapping the player's domain of actions to integers.
  num_players : int
    The number of players in the game.
  payoffs : Dict[int, numpy.ndarray]
    Dictionary mapping every player to their payoff array. The indices of the
    matrix correspond are stored in `action_to_index`.
  player_actions : Dict[int, List[Any]]
    A dictionary mapping every player to the list of actions she has
    available.
  **kwargs
    Arbitrary keyword arguments.
    
  Raises
  ------
  ValueError
    If the number of action options does not equal the number of players.
    If there are outcomes for whom no utility has been provided.
    If there is some outcome for which the number of payoff elements does not
    match the number of players in the game.
  
  """
  
  def __init__(self, players: List[Any], actions: List[Tuple[Any, ...]],
               payoff_function: Dict[Tuple[Any, ...], Tuple[float, ...]],
               **kwargs) -> None:
    self.players = players
    self.num_players = len(self.players)
    
    # assign actions to players
    if len(actions) != self.num_players:
      raise ValueError("the number of action options provided must equal the \
        number of players")
    self.player_actions = {}
    for i in range(self.num_players):
      self.player_actions[i] = actions[i]
      
    # map actions to integers for every player
    self.action_to_index = {}
    for i in range(self.num_players):
      self.action_to_index[i] = {}
      for j, a in enumerate(self.player_actions[i]):
        self.action_to_index[i][a] = j
        
    # build payoff arrays
    self.payoffs = {}
    payoff_array_shape = tuple([len(acts) for acts in \
                                self.player_actions.values()])
    for i in range(self.num_players):
      self.payoffs[i] = np.zeros(shape=payoff_array_shape)
    
    # check that all the possible outcomes are included in the payoff
    input_outcomes = set(payoff_function.keys())
    all_outcomes = self.all_outcomes()
    difference = all_outcomes - input_outcomes
    if difference != set():
      raise ValueError("outcomes {} not included in the payoff function"\
                       .format(difference))
        
    for rewards in payoff_function.values():
      if len(rewards) != self.num_players:
        raise ValueError("payoff {} should have as many elements as players \
                         there are in the game".format(rewards))
    # function
    for joint_actions, rewards in payoff_function.items():
      joint_indexes = self.actions_to_indices(*joint_actions)
      for i in range(self.num_players):
        self.payoffs[i][joint_indexes] = rewards[i]
        
    # store additional keyword arguments
    for k, v in kwargs.items():
      setattr(self, k, v)
        
  def actions_to_indices(self, *args) -> Tuple[int, ...]:
    r"""Map a list of joint actions to a list of integers.

    Parameters
    ----------
    *args
      Joint actions taken by the players.

    Returns
    -------
    indices : Tuple[int, ...]
      Tuple of integer indices corresponding to the joint actions.
      
    Raises
    ------
    ValueError
      If the number of actions does not match the number of players in the 
      game.

    """
    if len(args) != self.num_players:
      raise ValueError("number of actions must match number of players")
    indices = tuple(self.action_to_index[i][a] for i, a in enumerate(args))
    return indices
  
  def pure_strategies_rewards(self, *args) -> Tuple[float, ...]:
    r"""Get the rewards for every player when they play a pure strategy.
    
    Parameters
    ----------
    *args
      Joint actions taken by the players.

    Returns
    -------
    round_rewards : Tuple[float, ...]
      Rewards obtained by the players.

    """
    action_indices = self.actions_to_indices(*args)
    round_rewards = tuple(self.payoffs[i][tuple(action_indices)] for i in 
                     range(self.num_players))
    return round_rewards
  
  def num_outcomes(self) -> int:
    r"""Compute how many different outcomes are possible in this game.
    
    Returns
    -------
    int
      The number of possible outcomes.

    """
    num_outcomes = 1
    for player_actions in self.player_actions.values():
      num_outcomes *= len(player_actions)
    return num_outcomes
  
  def all_outcomes(self) -> Set[Tuple[Any, ...]]:
    r"""Generate all the possible outcomes as the product of player's actions.

    Returns
    -------
    all_outcomes : Set[Tuple[Any, ...]]
      A set of all the outcomes, as tuples of player's actions.

    """
    all_outcomes = set(product(*self.player_actions.values()))
    return all_outcomes
  
  def is_zero_sum(self) -> bool:
    r"""Check whether the game is zero-sum.
    
    A normal form game is *zero sum* if the payoff functions fulfill the
    following property:
      .. math:: \sum\limits_{i} u_i(a_1,...,a_n) = 0\quad\forall i\in N,\
        \forall a_1,...,a_n \in A
    
    Returns
    -------
    bool
      Is the game zero-sum(`True`) or not (`False`).

    """
    for z in self.all_outcomes():
      total_payoff = sum(self.pure_strategies_rewards(*z))
      if total_payoff != 0.:
        return False
    return True
  
  def __check_valid_mixed_strategy(self, mixed_strategy: Dict[Any, float]) \
    -> None:
    r"""Check if a mixed strategy is a valid probability distribution.    

    Parameters
    ----------
    mixed_strategy : Dict[Any, float]
      Map from action to probability.

    Raises
    ------
    ValueError
      If the sum of probabilities across actions does not equal unity within
      a tolerance of 1.E.3.

    """
    if not np.isclose(sum(mixed_strategy.values()), 1., atol=1.E-3):
      raise ValueError("sum of probability distribution over actions must \
                       equal 1 with tolerance 1.E-3")
  
  def mixed_strategy_support(self, player: int,
                             mixed_strategy: Dict[Any, float]) -> Set[Any]:
    r"""Get the support set of a given player's mixed strategy.
    
    Parameters
    ----------
    player : int
      Who is playing the mixed strategy. Takes values :math:`1, 2,...,n`
    mixed_strategy : Dict[Any, float]
      Probability distribution.

    Returns
    -------
    Set[Any]
      The set of actions with non-zero probability in the mixed strategy.

    """
    self.__check_valid_mixed_strategy(mixed_strategy)
    i = player-1
    support = set()
    for a in self.player_actions[i]:
      try:
        p_a = mixed_strategy[a]
      except KeyError:
        p_a = 0
      if p_a > 0:
        support.add(a)
    return support
    
  def mixed_strategies_rewards(self, *args) \
    -> Tuple[float, ...]:
    r"""Return the expected rewards from a mixed strategy profile.
    
    .. math:: u_i(s) = \sum\limits_{a\in A} u_i(a) \prod\limits_{j\in N} \
      s_j(a_j)
    
    Parameters
    ----------
    args
      Players' mixed strategies, as dictionaries mapping actions to
      probabilities. For every mixed strategy, it is checked that it is a
      valid probability distribution.
    """
    for ms in args:
      self.__check_valid_mixed_strategy(ms)
    rewards = tuple(0 for _ in range(self.num_players))
    players_actions = [mixed_strat.keys() for mixed_strat in args]
    pure_action_profiles = product(*players_actions)

    for pap in pure_action_profiles:
      probabilities = [args[i][a] for i, a in enumerate(pap)]
      joint_probability = np.prod(probabilities)
      joint_rewards = self.pure_strategies_rewards(*pap)
      rewards = tuple(r+joint_probability*jr for r, jr in
                      zip(rewards, joint_rewards))
    return rewards

  def switch_incentive(self, player: Any, action: Any,
                       mixed_strategy: Dict[Any, Dict[Any, float]]) -> float:
    r"""Compute the incentive for a player to switch to a pure strategy.
    
    Given a general mixed strategy profile, compute the incentive of
    ``player`` to switch to a pure strategy where ``action`` is played. It
    corresponds to the formula:
      
    .. math::
      
      c_{i}^{j}(s) = u_i(a_{j}^{i}, s_{-j}) - u_i(s) \\
      d_{i}^{j}(s) = max(c_{i}^{j}(s), 0)
    
    where the subindex corresponds to the :math`i`-th player and the
    superindex corresponds to its :math`j`-th action.

    Parameters
    ----------
    player : Any
      The player for whom the incentive is computed.
    action : Any
      The action that the player is tempted of swictching to as a pure
      strategy.
    mixed_strategy : Dict[Any, Dict[Any, float]]
      The original mixed strategy for all players.

    Returns
    -------
    float
      :math:`d_{i}^{j}(s)`.
      
    References
    ----------
    Shohan, Y., & Leyton-Browm, K. (2009). Computing Solution Concepts of
    Normal-Form Games. In Multiagent Systems: Algorithmic, Game-Theoretic,
    and Logical Foundations (pp. 87–112). Cambridge University Press.

    """
    for ms in mixed_strategy.values():
      self.__check_valid_mixed_strategy(ms)
    
    # utility under the original mixed strategy
    ms = [mixed_strategy[p] for p in self.players]
    mixed_strat_utility = self.mixed_strategies_rewards(*ms)
    
    # utility if player switches to pure strategy
    player_index = self.players.index(player)
    player_pure_strat = {a:0 for a in self.player_actions[player_index]}
    player_pure_strat[action] = 1
        
    switch_strat = [mixed_strategy[p] for p in self.players]
    switch_strat[player_index] = player_pure_strat
    switch_strat_utility = self.mixed_strategies_rewards(*switch_strat)

    c = switch_strat_utility[player_index] - mixed_strat_utility[player_index]
    if c>0:
      return c
    return 0.
  
  def incentive_target_function(self, 
    mixed_strategy: Dict[Any, Dict[Any, float]]) -> float:
    r"""Compute the target function to minimize the incentive to swicth.
    
    Compute the target function that is to be minimised when all players do
    not have an incentive to switch from the equilibrium mixed strategy.
    
    .. math::
      
      f(s) = \sum\limits_{i \in G} \sum\limits_{j \in A_j} (d_i^j(s))^2 

    Parameters
    ----------
    mixed_strategy : Dict[Any, Dict[Any, float]]
      The mixed strategy for which the total switching incentive is being
      computed.

    Returns
    -------
    float
      :math:`f(s)`.
      
    References
    ----------
    Shohan, Y., & Leyton-Browm, K. (2009). Computing Solution Concepts of
    Normal-Form Games. In Multiagent Systems: Algorithmic, Game-Theoretic,
    and Logical Foundations (pp. 87–112). Cambridge University Press.

    """
    f = 0.
    for n, p in enumerate(self.players):
      for a in self.player_actions[n]:
        incentive = self.switch_incentive(p, a, mixed_strategy)
        f += incentive**2
    return f
    
  def __str__(self) -> str:
    r"""For printing the game to console, only for two-player games.
    
    Returns
    -------
    str
      String representation of the game matrix.

    """
    if self.num_players != 2:
      return "Normal Form Game"
    actions1 = self.action_to_index[0].keys()
    actions2 = self.action_to_index[1].keys()
    numpy_matrix = np.empty(shape=(len(actions1), len(actions2)))*np.nan
    matrix = pd.DataFrame(numpy_matrix, index=actions1, columns=actions2,
                          dtype=object)
    for a1, a2 in product(actions1, actions2):
      outcome = self.pure_strategies_rewards(a1, a2)
      matrix.at[a1, a2] = outcome
    return matrix.to_string(justify='center') + "\n"
     
      
R, T, S, P = 6., 9., 0., 3.

prisoners_dilemma = NormalFormGame(
  players=['alice', 'bob'],
  actions=[(True, False)]*2,
  payoff_function = {
    (True, True): (R, R),
    (True, False): (S, T),
    (False, True): (T, S),
    (False, False): (P, P)
  }
)

rock_paper_scissors = NormalFormGame(
  players=['alice', 'bob'],
  actions=[('Rock', 'Paper', 'Scissors')]*2,
  payoff_function={
    ('Rock', 'Rock'): (0, 0),
    ('Rock', 'Paper'): (-1, 1),
    ('Rock', 'Scissors'): (1, -1),
    ('Paper', 'Rock'): (1, -1),
    ('Paper', 'Paper'): (0, 0),
    ('Paper', 'Scissors'): (-1, 1),
    ('Scissors', 'Rock'): (-1, 1),
    ('Scissors', 'Paper'): (1, -1),
    ('Scissors', 'Scissors'): (0, 0)
  }
)

if __name__ == '__main__':
  ex = NormalFormGame(
    players=['alice', 'bob'],
    actions=[('T', 'B'), ('L', 'C', 'R')],
    payoff_function={
      ('T','L'): (0,1),
      ('T','C'): (1,0),
      ('T','R'): (0,0),
      ('B','L'): (0,0),
      ('B','C'): (0,0),
      ('B','R'): (1,0),
    }
  )
  
  print(ex)
 