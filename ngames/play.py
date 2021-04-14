#!/usr/bin/env python3
# -*- coding: utf-8 -*-
r"""Interface to play a game based on a Prolog description.

This script implement the interaction between a set of players as spcified by
a set of Prolog files, through the terminal. It is intended to be executed
from the command line with the following arguments:
  
  $ python3 play.py [-h] [-dir DIR] [-id ID] [-t T] [-n N]

with the following arguments:
  -dir: directory where the Prolog files with the action situation
  description.
  -id: action situation identifier, which effectively selects the
  if-then-where susceptible of being activated.
  -t: the priority threshold of rules to consider. Rules whose priority
  exceeds this threshold are not considered.
  -n: the maximum number of rounds to be played. The game might be ended
  before if the termination conditions are fulfilled.

"""

import argparse
import numpy as np
np.random.seed(42)
from pyswip import Prolog
from copy import deepcopy
from pathlib import Path


explanation = "Simulate the interaction agents in an action arena governed \
  by the rules provided in the directory's Prolog files."
parser = argparse.ArgumentParser(description=explanation)
parser.add_argument('-dir', type=str, help='rule description directory',
                    required=True)
parser.add_argument('-id', type=str, help='action arena identifier',
                    required=True)
parser.add_argument('-t', type=int, help='rule priority threshold \
                    (default 1000)', default=1000)
parser.add_argument('-n', type=int, help='maximum rounds to be played \
                    (default 100)', default=100)

args = parser.parse_args()
directory = args.dir
identifier = args.id

threshold = args.t
max_rounds = args.n

print(identifier)

prolog = Prolog()

script_path = Path(__file__).parent.absolute()
prolog.consult("{}/general.pl".format(script_path))
prolog.consult("{}/agents.pl".format(directory))
prolog.consult("{}/rules.pl".format(directory))
prolog.consult("{}/states.pl".format(directory))

# STEP 1: get the participants
q = prolog.query("get_participants({},{},L)".format(identifier, threshold))
print("get_participants({},{},L)".format(identifier, threshold))
q_list = list(q)
q.close()
assert len(q_list) == 1
sln = q_list[0]['L']
participants = [x.value for x in sln]

print()
print("Participants are:")
print("-----------------")
for p in participants:
  print(p)
  prolog.assertz("participates({})".format(p))
print()

# STEP 2: assign participants to roles
q = prolog.query("get_roles({},{},L)".format(identifier, threshold))
q_list = list(q)
q.close()
assert len(q_list) == 1
sln = q_list[0]['L']
roles = {}
for x in sln:
  participant = x.args[0].value
  roles_list = x.args[1]
  p_roles = [y.value for y in roles_list]
  roles[participant] = p_roles

print("The roles participants take are:")
print("--------------------------------")
for p, p_roles in roles.items():
  print("{}:".format(p))
  for role in p_roles:
    prolog.assertz("role({},{})".format(p, role))
    print("\t{}".format(role))
print()

# STEP 3: find initial conditions
q = prolog.query("initially(Fact)")
current_state = []
for sln in q:
  current_state.append(sln['Fact'])
q.close()
# state facts sorted alphabetically
current_state.sort()

# LOOP: engage consecutive rounds
current_rounds = 1
while True:
  title = "***   STATE {}   ***".format(current_rounds)
  print("\n\n{}".format('*'*len(title)))
  print(title)
  print("{}\n".format('*'*len(title)))
  
  print("The state is given by:")
  print("----------------------")
  for f in current_state:
    prolog.assertz(f)
    print(f)
  print()
  
  # Check if the current state is terminal
  q = prolog.query("terminal")
  list_q = list(q)
  q.close()
  is_terminal = bool(len(list_q))
  if is_terminal:
    print("The current state is terminal. Play is finished.\n")
    break
  
  # Get the available actions
  q = prolog.query("get_actions({},{},L)".format(identifier, threshold))
  q_list = list(q)
  q.close()
  assert len(q_list) == 1, "Prolog found {} action lists, not 1"\
    .format(len(q_list))
  sln = q_list[0]['L']
  if not sln:
    print("No available actions. Play is finished.\n")
    break
  actions = {}
  print("The available actions are:")
  print("--------------------------")
  for x in sln:
    participant = x.args[0].value
    actions_list = x.args[1]
    p_actions = [y.value for y in actions_list]
    actions[participant] = p_actions
    print("{}:".format(participant))
    for a in p_actions:
      print("\t{}".format(a))
  print()
  
  # Choose actions
  print("Choose actions:")
  for p in actions.keys():
    p_action = input("\t{}: ".format(p))
    while p_action not in actions[p]:
      print("\tAction not recognized")
      p_action = input("\t{}: ".format(p))
    prolog.assertz("does({},{})".format(p, p_action))
  
  # Sample the next state
  q = prolog.query("process_control({},{},L)".format(identifier, threshold))
  q_list = list(q)
  q.close()
  assert len(q_list) == 1
  sln = q_list[0]['L']
  
  next_state = []
  probability = 1
  for x in sln:
    consequences_prob = x.args[1]
    probabilities = [y[1] for y in consequences_prob]
    consequences = [tuple(f.value for f in y[0]) for y in consequences_prob]
    i = np.random.choice(np.arange(len(consequences)), p=probabilities)
    new_facts = consequences[i]
    new_prob = probabilities[i]
    for f in new_facts:
      q = prolog.query("compatible({},[{}])".format(f,','.join(next_state)))
      list_q = list(q)
      q.close()
      is_compatible = bool(len(list_q))
      # if one fact of the consequences is not compatible, the whole set of
      # consequences for that rule and probability cannot be added
      if not is_compatible:
        break
    if is_compatible:
      _ = [next_state.append(f) for f in new_facts]
      probability *= new_prob
      
  # compatible facts from the pre-transition state are kept
  for old_fact in current_state:
    q = prolog.query("compatible({},[{}])".format(old_fact,
                                                  ','.join(next_state)))
    list_q = list(q)
    q.close()
    is_compatible = bool(len(list_q))
    if is_compatible:
      next_state.append(old_fact)

  for f in current_state:
    prolog.retractall(f)
    
  current_state = deepcopy(next_state)
  # state facts sorted alphabetically
  current_state.sort()
  prolog.retractall("does(_,_)")
  current_rounds += 1
  
  if current_rounds >= max_rounds:
    print("Maximum rounds exceeded.")
    break
  
prolog.retractall("role(_,_)")
prolog.retractall("participates(_)")
