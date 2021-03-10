# Agents

import sys
sys.path.append('/Users/nmontes/OneDrive/Documentos/PhD/ngames/ngames')
from ipywidgets import widgets

agents = []

out = widgets.Output()

def update_output():
    with out:
        out.clear_output()
        print("Your agents:")
        print("------------")
        for a in agents:
            print("  {}".format(a))

input_agent_label = widgets.Label("Introduce new agent:")
input_agent = widgets.Text(value='alice')

add_agent_button = widgets.Button(description='Add agent')
def add_agent(_):
    if input_agent.value not in agents:
        agents.append(input_agent.value)
        update_output()
add_agent_button.on_click(add_agent)

remove_agent_button = widgets.Button(description='Remove agent')
def remove_agent(_):
    if input_agent.value in agents:
        agents.remove(input_agent.value)
        update_output()
remove_agent_button.on_click(remove_agent)

clear_agents_button = widgets.Button(description='Clear all')
def clear_agents(_):
    agents.clear()
    update_output()
clear_agents_button.on_click(clear_agents)

row1 = widgets.HBox([input_agent_label, input_agent])
row2 = widgets.HBox([add_agent_button, remove_agent_button,
                     clear_agents_button])

widgets.VBox([row1, row2, out])

agents