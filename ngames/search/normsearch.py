from typing import Callable, Sequence, Dict
import subprocess
from ngames.evaluation.build import build_full_game
from ngames.evaluation.extensivegames import ExtensiveFormGame, plot_game
from ngames.evaluation.equilibrium import minimize_incentives,\
    subgame_perfect_equilibrium, outcome_probability

# TODO documentation
# TODO Simulated Annealing search algorithm
# TODO test search with fishers benchmark


def utilitarian_social_welfare(utilities: Sequence[float]) -> float:
    return sum(utilities)


class NormSearch:
    def __init__(self, defaults: str, norm_repository: str,
                 welfare_function: Callable[[Sequence[float]], float],
                 id: str, norms: Sequence[str]) -> None:
        self.path_to_default = defaults
        self.path_to_norm_repo = norm_repository
        self.evaluation_function = welfare_function
        self.id = id
        self.norms_in_repo = norms
        self.N = len(norms)

    def build_game(self, normative_system: Dict[str, int]) \
            -> ExtensiveFormGame:
        # first turn normative system dict into a string
        if normative_system == {}:
            ns_str = "ns{}"
        else:
            ns_str = 'ns{'
            for norm, priority in normative_system.items():
                ns_str += "{}:{},".format(norm, priority)
            ns_str = ns_str[:-1] + '}'
        subprocess.run(
            ["bash", "ngames/search/writer.sh",
             self.path_to_default, self.path_to_norm_repo, ns_str, self.id]
        )
        game = build_full_game('.', self.id)
        subprocess.run(["rm", "agents.pl", "states.pl", "rules.pl"])
        return game

    def evaluate_game(self, game: ExtensiveFormGame) -> float:
        subgame_mixed_strat, _, _ = subgame_perfect_equilibrium(
            game, minimize_incentives)
        outcome_prob = {t: outcome_probability(game, subgame_mixed_strat, t)
                        for t in game.game_tree.terminal_nodes}
        social_welfare = {t: self.evaluation_function(game.utility[t].values())
                          for t in game.game_tree.terminal_nodes}
        score_per_node = {t: outcome_prob[t]*social_welfare[t]
                          for t in game.game_tree.terminal_nodes}
        score = sum(score_per_node.values())
        return score


if __name__ == '__main__':
    search = NormSearch(
        defaults="fishers_example_small",
        norm_repository="fishers_example_small/norms.pl",
        welfare_function=utilitarian_social_welfare,
        id="fishers",
        norms=["firstInTime", "firstToAnnounce"]
    )

    game = search.build_game({'firstToAnnounce': 1})
    sw = search.evaluate_game(game)
    print(sw)

    my_fig_kwargs = dict(figsize=(20, 10), frameon=False)
    my_node_kwargs = dict(font_size=10, node_size=500, edgecolors='k',
                          linewidths=1.5)
    my_edge_kwargs = dict(arrowsize=15, width=1.5)
    my_edge_labels_kwargs = dict(font_size=10)
    my_patch_kwargs = dict(linewidth=2)
    my_legend_kwargs = dict(fontsize=16, loc='upper right', edgecolor='white')
    my_info_sets_kwargs = dict(linestyle='--', linewidth=3)
    my_utility_label_kwargs = dict(horizontalalignment='center', fontsize=10,
                                   weight='bold')

    position_colors = {'alice': 'aquamarine', 'bob': 'greenyellow'}

    fig = plot_game(game,
                    position_colors,
                    fig_kwargs=my_fig_kwargs,
                    node_kwargs=my_node_kwargs,
                    edge_kwargs=my_edge_kwargs,
                    edge_labels_kwargs=my_edge_labels_kwargs,
                    patch_kwargs=my_patch_kwargs,
                    legend_kwargs=my_legend_kwargs,
                    utility_label_kwargs=my_utility_label_kwargs,
                    utility_label_shift=0.08,
                    decimals=0,
                    info_sets_kwargs=my_info_sets_kwargs)
