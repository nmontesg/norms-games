#!/usr/bin/env python3
# -*- coding: utf-8 -*-
r"""Build a complete Extensive Form Game from an action situation description.

This script includes all the functions to create a complete Extensive Form
Game from a Prolog description using the syntax of if-then-where rules.

"""

import logging
import networkx as nx
from pyswip import Prolog
from pathlib import Path
from extensivegames import ExtensiveFormGame
from copy import deepcopy
from typing import List, Dict, Tuple, Set, Any


prolog = Prolog()

def get_participants(identifier: str, threshold: int) -> List[str]:
  r"""Get the set of partiticipants.

  Query the Prolog system to extract the list of participants allowed to take
  part in the game according to the boundary rules.

  Parameters
  ----------
  identifier : str
    Identifier of the action situation under consideration.
  threshold : int
    Boundary rules of priority exceeding this threshold are not considered.

  Returns
  -------
  participants : List[str]

  """
  q = prolog.query("get_participants({},{},L)".format(identifier, threshold))
  q_list = list(q)
  q.close()
  assert len(q_list) == 1, "Prolog found {} participant lists, not 1"\
    .format(len(q_list))
  sln = q_list[0]['L']
  return sln


def get_roles(identifier: str, threshold: int) -> Dict[str, List[str]]:
  r"""Get the roles that the participants assume.

  Query the Prolog system to extract the list of roles for every participant,
  according to the position rules. One participant may assume several roles.
  An agent who has been denoted as a participant but is not assigned any role
  is equivalent to not participating at all.

  Parameters
  ----------
  identifier : str
    Identifier of the action situation under consideration.
  threshold : int
    Position rules of priority exceeding this threshold are not considered.

  Returns
  -------
  roles : Dict[str, List[str]]
    A dictionary with participants as keys and their list of assigned roles
    as values.

  """
  q = prolog.query("get_roles({},{},L)".format(identifier, threshold))
  q_list = list(q)
  q.close()
  assert len(q_list) == 1, "Prolog found {} roles lists, not 1"\
    .format(len(q_list))
  sln = q_list[0]['L']
  return sln


def get_initial_conditions() -> List[str]:
  r"""Get the initial conditions of the action situation.

  Query the Prolog system to extract the initial conditions declared with the
  predicate initially/1. This call must be performed once the participants and
  their assigned roles have been asserted into the system.

  Returns
  -------
  initial_facts : List[str]

  """
  q = prolog.query("initially(Fact)")
  initial_facts = []
  for sln in q:
    initial_facts.append(sln['Fact'])
  q.close()
  initial_facts.sort()
  return initial_facts


def get_actions(identifier: str, threshold: int) -> Dict[str, List[str]]:
  r"""Get the available actions for all the participants.

  Query the Prolog system to get the actions available to every participant
  considering the current facts of the state.

  Parameters
  ----------
  identifier : str
    Identifier of the action situation under consideration.
  threshold : int
    Choice rules of priority exceeding this threshold are not considered.

  Returns
  -------
  actions : Dict[str, List[str]]
    Dictionary with the participants as keys and the lists of the actions
    available to them as values.

  """
  q = prolog.query("get_actions({},{},L)".format(identifier, threshold))
  q_list = list(q)
  q.close()
  assert len(q_list) == 1, "Prolog found {} action lists, not 1"\
    .format(len(q_list))
  sln = q_list[0]['L']
  actions = {}
  for x in sln:
    participant = x.args[0].value
    action = x.args[1].value
    try:
      actions[participant].append(action)
    except KeyError:
      actions[participant] = [action]
  return actions


def build_game_subtree(game: ExtensiveFormGame, actions: Dict[str, List[str]],
                       expand_node: int, node_counter: int) \
  -> Tuple[Set[int], int]:
  r"""Build the game subtree emanating from a particular node.

  Given a game whose tree is to be expanded, the node where the expansion is
  to take place and the dictionary of actions available at the expansion
  point, grow the tree at that node and handle the information sets since
  participants are assumed to take actions simultaneously.

  Parameters
  ----------
  game : ExtensiveFormGame
    The game to be expanded. It is not returned since it is modified through
    class methods.
  actions : Dict[str, List[str]]
    The dictionary of actions available at the expansion point.
  expand_node : int
    The node where the expansion is to be appended.
  node_counter : int
    To keep count of the nodes in the game tree.

  Returns
  -------
  Tuple[Set[int], int]
    Return the set of the nodes added at the deepest level and the update node
    counter.

  """
  actor_info_set = {expand_node}
  for actor, actions in actions.items():
    for n in actor_info_set:
      game.set_node_player(n, actor)
    game.add_information_sets(actor, actor_info_set)
    next_actor_info_set = set()
    for n in actor_info_set:
      for a in actions:
        game.add_node(node_counter)
        game.add_edge(n, node_counter, a)
        next_actor_info_set.add(node_counter)
        node_counter += 1
    actor_info_set = deepcopy(next_actor_info_set)
  return actor_info_set, node_counter


def get_control_rule_consequences(game: ExtensiveFormGame, identifier: str,
                                  threshold: int, expand_node: int, n: int) \
  -> List[Any]:
  r"""Get the control rule consequences at some node.

  Given two nodes `expand_node` (a ``parent'') and `n` (a ``child''),
  get the actions necessary to move form the parent to the child, assert them
  into the Prolog system, get the control rule consequences and return them.

  Parameters
  ----------
  game : ExtensiveFormGame
    Game being built.
  identifier : str
    Identifier of the action situation under consideration.
  threshold : int
    Control rules of priority exceeding this threshold are not considered.
  expand_node : int
    Node at whom some facts are known, and from which some actions have been
    performed.
  n : int
    Node towards whom the system evolves after taking some actions at the
    previous node.

  Returns
  -------
  sln : List[Any]
    List of the processed control rule consequences of performing actions
    needed to go from `expand_node` to `n`, considering the currently true
    facts at `expand_node`. The Prolog predicate that processes the query
    returns the consequences sorted from those derived from rules with higher
    to lower priority.

  """
  # assert the actions to get to that node
  path = nx.bidirectional_shortest_path(game.game_tree, expand_node, n)
  
  for i in range(len(path[:-1])):
    who = game.turn_function[path[i]]
    what = game.game_tree.get_edge_data(path[i], path[i+1])
    prolog.assertz("does({},{})".format(who, what['action']))
  # retrieve the facts at the node
  q = prolog.query("process_control({},{},L)".format(identifier, threshold))
  q_list = list(q)
  q.close()
  assert len(q_list) == 1, "Prolog found {} control rule consequences lists,\
    not 1".format(len(q_list))
  sln = q_list[0]['L']
  return sln


def is_rule_compatible(rule_consequences: List[Any], next_states: List[Any]) \
  -> bool:
  r"""Determine whether a control rule is to be considered.

  A control rule, in general, when instantiated, gives rise to many
  consequences with different degrees of probability. These consequences, in
  turn, are conjunction of many facts. A control new can only be considered
  to be processed into the facts already derived by control rules of higher
  priority if any of the facts in any of its consequences is incompatible
  with any of the facts of any of the already derived potential new states.
  Otherwise, the probability distribution over a chance node leading to many
  new states might not add up to unity.

  Parameters
  ----------
  rule_consequences : List[Any]
    The consequences of a control rule under consideration to be added to the
    already derived facts.
  next_states : List[Any]
    The states that have already been derived (albeit incomplete) from control
    rules of higher priority.

  Returns
  -------
  bool
    Whether the consequences of the control rules are compatible with the
    previously derived facts.

  """
  # loop over the potential new states
  for s in next_states:
    # loop over the potential additional consequences
    for conseq in rule_consequences:
      # loop over the individial facts in the potential new consequences
      for f in conseq:
        q = prolog.query("compatible({},[{}])".format(f,','.join(s)))
        list_q = list(q)
        q.close()
        is_compatible = bool(len(list_q))
        if not is_compatible:
          return False
  return True


def add_rule_consequences(rule_consequences: List[Any],
                          consequences_probs: List[float],
                          next_states_facts: List[Any],
                          next_states_probs: List[float]) \
  -> Tuple[List[Any], List[float]]:
  r"""Incorporate new control rules consequences into the already derived.

  Given a set of control rules consequences compatible with the ones already
  derived, and their respective probabilities, incorporate the facts of the
  most recently actiavted control rules and update the resulting probabilities
  as the product of the probability of the (incomplete) fact already derived
  and the probability of the consequence set of the new rule.

  Parameters
  ----------
  rule_consequences : List[Any]
    List of consequences of the most recently activated control rule.
  consequences_probs : List[float]
    List of probabilities of the consequences of the most recently activated
    control rule.
  next_states_facts : List[Any]
    List of consequences of the already derived new states, not complete yet.
  next_states_probs : List[float]
    List of the probabilities of the already derived new states.

  Returns
  -------
  Tuple[List[Any], List[float]]
    List with the updated new potential states, and the list of the updated
    probabilities for those new potential states.

  """
  updated_next_states = []
  updated_next_states_probs = []
  for i in range(len(rule_consequences)):
    for j in range(len(next_states_facts)):
      updated_next_states.append(
        [*rule_consequences[i], *next_states_facts[j]])
      updated_next_states_probs.append(
        consequences_probs[i]*next_states_probs[j])
  return updated_next_states, updated_next_states_probs


def is_terminal() -> bool:
  r"""Check if the termination conditions are met.

  Returns
  -------
  bool

  """
  q = prolog.query("terminal")
  list_q = list(q)
  q.close()
  return bool(len(list_q))


def build_full_game(folder: str, identifier: str, threshold: int=1000,
                    max_rounds: int=10, module: str="default", 
                    verbose: bool=False) \
  -> ExtensiveFormGame:
  r"""Build a complete game tree from an action situation Prolog description.

  This function takes in a complete Prolog description of an action situation
  according to the if-then-where rules and builds the complete Extensive Form
  Game that models the situation. It expands the game tree in a breadth-first
  fashion.

  Additional attributes of the game not included in the original
  implementation are added to the returned game:
    - roles: a dictionary mapping the participants to the list of roles they
    assume.
    - node_rounds: a dictionary mapping every ``non-intermediate'' node
    to the number of rounds performed to get there.
    - node_info: a dictionary mapping every ``non-intermediate'' node to
    the predicates that hold true in that node.

  Parameters
  ----------
  folder : str
    Folder where the agents.pl, states.pl and rules.pl files are placed.
  identifier : str
    Identifier of the action situation under consideration.
  threshold : int
    if-then-where rules, of any type, whose priority exceeds the threshold
    are not considered while building the game.
  max_rounds : int, optional
    The maximum number of rounds to perform during the game tree expansion.
    The default is 10.
  verbose : bool, optional
    Whether the evolution of the game expansion should be saved to file called
    build.log. Only when the the script using the function is called from the
    terminal.

  Returns
  -------
  ExtensiveFormGame
    The resulting Extensive Form Game corresponding to the action situation.

  """
  script_path = Path(__file__).parent.absolute()
  prolog.consult("{}/interpreter.pl".format(script_path))
  prolog.consult("{}/agents.pl".format(folder))
  prolog.consult("{}/rules.pl".format(folder))
  prolog.consult("{}/states.pl".format(folder))

  game = ExtensiveFormGame(ID=identifier)

  if verbose:
    logging.basicConfig(format='%(message)s',
                        level=logging.DEBUG,
                        filename='build.log', filemode='w')
  else:
    logging.basicConfig(format="%(levelname)s: %(message)s")

  # STEP 1: Get the participants and add them to the game
  participants = get_participants(identifier, threshold)
  for p in participants:
    game.add_players(p.args[0].value)
    prolog.assertz(p.value)
  logging.info("participants are: {}".format(participants))
  logging.info("")
  
  # STEP 2: Assign the participants to roles
  roles = get_roles(identifier, threshold)
  game.roles = {p:[] for p in game.players}
  for r in roles:
    player = r.args[0].value
    role= r.args[1].value
    try:
      game.roles[player].append(role)
    except KeyError:
      game.roles[player] = [role]
    prolog.assertz(r.value)
  logging.info("the roles assigned to participants are: {}\n".\
               format(game.roles))
  
  # STEP 3: Get initial state and add it as root node to game
  initial_facts = get_initial_conditions()
  node_counter = 1
  game.add_node(node_counter, is_root=True)
  game.node_info = {node_counter: initial_facts}
  game.node_rounds = {node_counter: 0}
  expand_queue = [node_counter]
  node_counter += 1

  # MAIN LOOP
  while expand_queue:
    # Get the node to be expanded
    logging.info("Expand queue: {}\n".format(expand_queue))
    expand_node = expand_queue.pop(0)
    logging.info("*** EXPANDING NODE {} ***".format(expand_node))
    if game.node_rounds[expand_node] >= max_rounds:
      logging.info("Rounds at node {} exceed maximum allowed\n\n".format(
        expand_node))
      continue
    expand_node_facts = game.node_info[expand_node]
    logging.info("Facts at node being expanded:")
    for f in expand_node_facts:
      logging.info("\t{}".format(f))
      prolog.assertz(f)
    logging.info("")

    # Check if the current node is terminal
    if is_terminal():
      logging.info("Termination conditions met at node {}\n\n"\
                   .format(expand_node))
      for f in expand_node_facts:
        prolog.retractall(f)
      continue

    # Get the available actions at node being expanded
    actions = get_actions(identifier, threshold)
    # if no actions are available: retrieve the facts and move to the
    # next node
    if actions == {}:
      logging.info("No actions available at node {}\n\n".format(expand_node))
      for f in expand_node_facts:
        prolog.retractall(f)
      continue
    logging.info("Available actions at node being expanded:")
    for p, a in actions.items():
      logging.info("\t{}: {}".format(p,a))
    logging.info("")

    # Build the game sub-tree emanating from the node being expanded
    provisional_next_states, node_counter = build_game_subtree(game, actions,
                                                               expand_node,
                                                               node_counter)
    logging.info("Subtree emanating from node {} built.".format(expand_node))
    logging.info("Provisional next states: {}\n"\
                 .format(provisional_next_states))

    # Process the facts at the provisional next states
    for n in provisional_next_states:
      logging.info("Processing provisional next state {}".format(n))
      sln = get_control_rule_consequences(game, identifier, threshold,
                                          expand_node, n)
      logging.info("{} activated control rules\n".format(len(sln)))

      # go through the activated control rules to generate the definitive
      # next states
      next_states = [[]]
      next_states_prob = [1]
      for active_rule in sln:
        consequences_prob = active_rule.args[1]
        probabilities = [y[1] for y in consequences_prob]
        consequences = [[f.value for f in y[0]] for y in consequences_prob]
        for c, p in zip(consequences, probabilities):
          logging.info("{} -- {}".format(c,p))
        logging.info("")

        # if it is not the first rule to be activated, first check that the
        # rule can be applied if there is not any contradiction between any
        # of the already established next states and any of the potential new
        # facts
        is_compatible = is_rule_compatible(consequences, next_states)
        if not is_compatible:
          continue

        # if the rule can be added to the already established facts
        new_next_states, new_next_states_prob = \
          add_rule_consequences(consequences, probabilities,
                                next_states, next_states_prob)
        next_states = deepcopy(new_next_states)
        next_states_prob = deepcopy(new_next_states_prob)

      # for every potential new state, keep the compatible facts from the
      # parent node
      for s in next_states:
        for f in expand_node_facts:
          q = prolog.query("compatible({},[{}])".format(f,','.join(s)))
          list_q = list(q)
          q.close()
          is_compatible = bool(len(list_q))
          if is_compatible:
            s.append(f)
            logging.info("keeping fact {} in potential next state {}\n"\
                         .format(f, s))

      logging.info("Potential states from node {}:".format(n))
      for s, p in zip(next_states, next_states_prob):
        logging.info("{} -- {}".format(s, p))
      logging.info("")

      # add the potential new states to the game tree
      assert len(next_states) >= 1, "{} next state consequences found".\
        format(len(next_states))
        
      # Check if the actions taken in conjunction with the pre-transition
      # state lead to termination
      termination = is_terminal()
      if termination:
        logging.info("Termination conditions met at node when going from \
                     node {} to node {}\n\n".format(expand_node, n))
                     
      # if the transition has been deterministic, then we do not need to add
      # new nodes
      if len(next_states) == 1:
        game.node_rounds[n] = game.node_rounds[expand_node]+1
        next_states[0].sort()
        game.node_info[n] = next_states[0]
        if not termination:
          expand_queue.append(n)
          logging.info("Node {} added to node queue\n".format(n))

      # if the transition has not been deterministic, set node to chance and
      # add new nodes
      else:
        game.set_node_player(n, 'chance')
        probability_distribution = {}
        for m, p in zip(next_states, next_states_prob):
          game.add_node(node_counter)
          game.add_edge(n, node_counter, label='')
          probability_distribution[(n, node_counter)] = p
          game.node_rounds[node_counter] = game.node_rounds[expand_node]+1
          m.sort()
          game.node_info[node_counter] = m
          if not termination:
            expand_queue.append(node_counter)
            logging.info("Node {} added to node queue\n".format(node_counter))
          node_counter += 1
        game.set_probability_distribution(n, probability_distribution)

      # clean the asserted actions to get to that node. They have been
      # asserted when calling get_control_rule_consequences
      prolog.retractall("does(_,_)")

    for f in expand_node_facts:
      prolog.retractall(f)

  prolog.retractall("role(_,_)")
  prolog.retractall("participates(_)")

  return game


if __name__ == '__main__':
  pass
