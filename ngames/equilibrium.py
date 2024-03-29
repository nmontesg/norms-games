r"""Compute equilibrium paths of play using backward induction.

Assume sub-game perfect rationality.
To be applied on extensive-form games that are equivalent to heterogenous
normal form games played sequentially, i.e. games that have been generated
from a set of Prolog files and the ``build.build_full_game`` function.

"""

from networkx.algorithms.dag import descendants
from networkx.algorithms.shortest_paths.generic import shortest_path
from itertools import product
from copy import deepcopy
from scipy.optimize import minimize
from ngames.extensivegames import ExtensiveFormGame
from ngames.normalgames import NormalFormGame
from typing import Callable, Dict, Any, Tuple, List


def build_subgame(extensive_game: ExtensiveFormGame, root: int) \
        -> NormalFormGame:
    r"""Build a normal game that emanates from a node in an extensive game.

    Assumes that the root from which the normal form game is built corresponds
    to the root of the last round of the extensive game.

    Parameters
    ----------
    extensive_game : ExtensiveFormGame
    root : int
        The root from which a last rounds of the extensive game starts.

    Raises
    ------
    AssertionError
        If a descendant of a chance node is not terminal.
    ValueError
        If a path of play does not end at a chance or at a terminal node.

    Returns
    -------
    normal_form_game : NormalFormGame
        The game in normal form corresponding that starts at the input root
        node.

    """
    # get the players and their possible actions
    player_actions = {}
    n = root
    while True:
        try:
            active_player = extensive_game.turn_function[n]
            if active_player == 'chance':
                break
        # KeyError happens if terminal node is reached, not in the turn
        # function
        except KeyError:
            break

        out_edges = list(extensive_game.game_tree.out_edges(n, data='action'))
        actions = tuple(o[2] for o in out_edges)
        player_actions[active_player] = actions
        n = out_edges[0][1]

    players = extensive_game.players
    actions = []
    for p in players:
        try:
            actions.append(player_actions[p])
        except KeyError:
            actions.append(('no-op',))

    # build the payoff function
    payoff_function = {}
    non_empty_actions = [a for a in actions if a]
    possible_play_paths = product(*non_empty_actions)

    # scan through the possible paths of play
    for p in possible_play_paths:
        n = root
        for action in p:
            if action == 'no-op':
                continue
            out_edges = list(extensive_game.game_tree.out_edges(n,
                                                                data='action'))
            next_node = [o[1] for o in out_edges if o[2] == action][0]
            n = next_node

        # last node of play path is a terminal node
        if n in extensive_game.game_tree.terminal_nodes:
            utility = tuple(extensive_game.utility[n][p] for p in players)

        # last node of play path is a chance node: weight average over
        # descendant terminal nodes
        elif extensive_game.turn_function[n] == 'chance':
            utility_dict = {pl: 0 for pl in players}
            for (_, desc), prob in extensive_game.probability[n].items():
                assert desc in extensive_game.game_tree.terminal_nodes,\
                    "node {} should be a terminal node".format(desc)
                for pl in players:
                    utility_dict[pl] += prob*extensive_game.utility[desc][pl]
            utility = tuple(utility_dict[pl] for pl in players)

        else:
            raise ValueError("node at end of path play {} from root node {} \
                        is not a terminal nor a chance node".format(p, root))

        payoff_function[p] = utility

    normal_form_game = NormalFormGame(
        players=players,
        actions=actions,
        payoff_function=payoff_function
    )

    return normal_form_game


def scalar_function(x: List[float]) -> float:
    r"""Function to be minimized: total deviation incentives for all players.

    Written in a format such that the only input is a mixed strategy encoded
    as a numpy.array.

    Parameters
    ----------
    x : List[float]
        Array encoding a mixed strategy. The game to which it is to be applied
        is set as an attribute of the function externally

    Raises
    ------
    AttributeError
        If, at the time of being called, no normal-form game has been set as an
        attribute, to which the mixed strategy array encoded in the array is
        passed to compute the incentives to deviate.

    Returns
    -------
    f : float
        The function to be minimixed, i.e. the total incentive to deviate from
        the mixed strategy:

        .. math::

            f(s) = \sum\limits_{i \in G} \sum\limits_{j \in A_j} (d_i^j(s))^2

    References
    ----------
    Shohan, Y., & Leyton-Browm, K. (2009). Computing Solution Concepts of
    Normal-Form Games. In Multiagent Systems: Algorithmic, Game-Theoretic,
    and Logical Foundations (pp. 87–112). Cambridge University Press.

    """
    if not scalar_function.game:
        raise AttributeError("scalar_function() called without game attribute")
    mixed_strat = scalar_function.game.strategies_vec2dic(x)
    f = scalar_function.game.incentive_target_function(mixed_strat)
    return f


def minimize_incentives(normal_game: NormalFormGame) \
        -> Tuple[Dict[Any, Dict[Any, float]], float]:
    r"""Compute the mixed strategy that minimizes the incentive to deviate.

    Given an arbitrary normal-form game, compute the (in general, mixed)
    strategy profile that minimizes the incentives to deviate from all players.
    The target function being minimized is:

    .. math::

        f(s) = \sum\limits_{i \in G} \sum\limits_{j \in A_j} (d_i^j(s))^2 \\
        c_{i}^{j}(s) = u_i(a_{j}^{i}, s_{-j}) - u_i(s) \\
        d_{i}^{j}(s) = max(c_{i}^{j}(s), 0)

    It uses the scipy.optimize library to solve the optimization problem. In
    particular, their implementation of the Trust-Region Constrained algorithm.

    Parameters
    ----------
    normal_game : NormalFormGame

    Raises
    ------
    ValueError
        If the optimization (i.e. the call to ``scipy.optimize.minimize``) is
        not successfull.

    Returns
    -------
    Dict[Any, Dict[Any, float]]
        The mixed strategy that minimizes the total incentives to deviate, as a
        dictionary mapping every player in the game to a mixed strategy.
    float
        The target function at the solution.

    """

    # set attribute of the function to minimize, since it can only take a
    # vector array as argument
    setattr(scalar_function, 'game', normal_game)

    x0 = normal_game.completely_random_strat_vector()
    linear_constraints = normal_game.make_linear_constraints()
    bounds = normal_game.make_strat_bounds()

    opt = minimize(scalar_function,
                   x0,
                   method='SLSQP',
                   constraints=[linear_constraints],
                   options={'ftol': 1.E-8},
                   bounds=bounds)

    delattr(scalar_function, 'game')

    # check that the minimization has been successful
    if not opt.success:
        raise ValueError(opt.message)

    # from vector to mixed strategy dictionary
    mixed_strat_solution = normal_game.strategies_vec2dic(opt.x)

    # return the mixed strategy and the value of the target function
    return mixed_strat_solution, opt.fun


def subgame_perfect_equilibrium(extensive_form_game: ExtensiveFormGame,
                                equilibrium_function: Callable) -> \
                                Tuple[Dict[Any, Any], Dict[Any, Any],
                                      Dict[Any, Any]]:
    r"""Compute the sequential equilibriums in an Extensive Form Game.

    This function works on an Extensive Form Game built as a sequence of,
    possibly different, Normal Form Games. This function identifies the
    subgames starting from those closer to the end of the game (i.e. to the
    root nodes), and passes them to ``build_subgame`` to build that part of
    the game tree as a normal form game. Then, it calls the provided
    equilibrium function to find the mixed strategy incentivized in that
    subgame, and backtracks the resulting utility up the game tree.

    Parameters
    ----------
    extensive_form_game : ExtensiveFormGame
    equilibrium_function : Callable
        The function that computes a solution concept on a (single-rounds)
        normal form game. It should return the result as a dictionary with
        the game players as keys, and a dictionary mapping their available
        actions to the probabilities as values.

    Returns
    -------
    subgame_mixed_strategies : Dict[Any, Any]
        The mapping from the nodes that are the roots of the subgames to the
        mixed equilibrium strategies computed for their subgame.
    backtrack_utilities : Dict[Any, Any]
        The mapping from the nodes that are the roots of the subgames to the
        utilities resulting from the computed equilibrium strategies.
    target_function : Dict[Any, Any]
        Mapping from the nodes that are roots of the sequential normal-form
        games to the optimized deviation incentive function, which should
        ideally be ~0 for all nodes.

    """
    extensive_game = deepcopy(extensive_form_game)
    max_rounds = max(extensive_game.node_rounds.values())
    subgame_mixed_strategies = {}
    backtrack_utilities = {}
    target_function = {}
    subgame_rounds = max_rounds-1

    while subgame_rounds >= 0:
        subgame_root_nodes = [n for n in extensive_game.node_rounds.keys()
                              if extensive_game.node_rounds[n]
                              == subgame_rounds
                              and n not in
                              extensive_game.game_tree.terminal_nodes]

        # compute equilibrium at the subgames closest to the terminal nodes
        for s in subgame_root_nodes:
            # build the last subgame as an extensive form game
            normal_game = build_subgame(extensive_game, s)

            # function to compute the mixed strategy equilibrium here
            mixed_equilibrium_strategy, f = equilibrium_function(normal_game)

            subgame_mixed_strategies[s] = mixed_equilibrium_strategy
            target_function[s] = f

            # store utility at the root node of the subgame
            mes_list = [mixed_equilibrium_strategy[p] for p in
                        normal_game.players]
            rewards = normal_game.mixed_strategies_rewards(*mes_list)
            backtrack_utilities[s] = {p: r for p, r in
                                      zip(normal_game.players, rewards)}

        for s in subgame_root_nodes:
            # delete all descendants from subgame root nodes
            desc = descendants(extensive_game.game_tree, s)
            extensive_game.game_tree.remove_nodes_from(desc)

            # remove previous terminal nodes from terminal list
            terminal_previous = [d for d in desc
                                 if d in
                                 extensive_game.game_tree.terminal_nodes]
            for t in terminal_previous:
                extensive_game.game_tree.terminal_nodes.remove(t)
            extensive_game.game_tree.terminal_nodes.append(s)

            # remove previous non-terminal nodes from turn function
            for d in desc:
                if d in extensive_game.turn_function.keys():
                    extensive_game.turn_function.pop(d)

            # remove subgame root node from turn function because now it is
            # terminal
            extensive_game.turn_function.pop(s)

            # backtrack utility
            extensive_game.set_utility(s, backtrack_utilities[s])

        subgame_rounds -= 1

    return subgame_mixed_strategies, backtrack_utilities, target_function


def outcome_probability(extensive_game: ExtensiveFormGame,
                        rounds_strat: Dict[Any, Any], outcome_node: Any) \
        -> float:
    r"""Compute the probability of reaching a terminal node.

    Compute the probability that a terminal node is reached given the
    strategies of players at the consecutive game rounds.

    Parameters
    ----------
    extensive_game : ExtensiveFormGame
        The game in extensive form where the path and probability of play is
        to be computed.
    rounds_strat : Dict[Any, Any]
        The mixed strategies at every round of the game, equivalent to a
        normal form game.
    outcome_node : Any
        The terminal node towards which the path of play is to be computed.

    Returns
    -------
    float

    """
    assert outcome_node in extensive_game.game_tree.terminal_nodes, \
        "node {} is not a terminal node".format(outcome_node)
    path_from_root = shortest_path(extensive_game.game_tree,
                                   extensive_game.game_tree.root, outcome_node)
    probability = 1
    for i in range(len(path_from_root)-1):
        n = path_from_root[i]
        # retrieve the strategy that applies to that part of the game
        if n in rounds_strat.keys():
            local_strategy = rounds_strat[n]

        player = extensive_game.turn_function[n]
        next_node = path_from_root[i+1]

        if player == 'chance':
            action_prob = extensive_game.probability[n][(n, next_node)]
            pass
        else:
            a = extensive_game.game_tree.get_edge_data(n, next_node, 'action')
            action = a['action']
            action_prob = local_strategy[player][action]
        probability *= action_prob
        if probability == 0:
            return 0
    return probability
