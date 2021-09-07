r"""Class for the evaluation module.

"""

from extensivegames import ExtensiveFormGame
from build import build_game_from_rule_combination
from typing import Dict, Callable, Any

class EvaluationModule:
  def __init__(self, action_situation_id: str, folder: str,
               utility_function: Callable[[ExtensiveFormGame], None],
               utility_function_kwargs: Dict[Any,Any] = None,
               value_system_function = None) -> None:
    self.action_situation_id = action_situation_id
    self.folder = folder
    self.utility_function = utility_function
    self.utility_function_kwargs = utility_function_kwargs,
    self.value_system_function = value_system_function
    
  def evaluate_normative_system(self, instance: Dict[str, int]) -> float:
    game = build_game_from_rule_combination(
      self.folder,
      self.action_situation_ididentifier,
      instance,
      utility_assignment_function = self.utility_function,
      utility_function_kwargs = self.utility_function_kwargs)

