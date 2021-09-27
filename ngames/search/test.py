import subprocess
import os

defaults_path = "ngames/search/backup"
norms_path = "ngames/search/norms.pl"

norms = ["firstInTime", "firstToAnnounce"]
i = 1
normative_system_dict = {}
for n in norms:
    normative_system_dict[n] = i
    i += 1

normative_system = '{'
for norm, priority in normative_system_dict.items():
    normative_system += "{}:{},".format(norm, priority)
normative_system = normative_system[:-1] + '}'
print(normative_system)

id = "fishers"

subprocess.run(
    ["bash", "ngames/search/writer.sh"],
    env=dict(os.environ, DEFAULTS=defaults_path, NORMS=norms_path))

# subprocess.run(["rm", "agents.pl", "states.pl", "rules.pl"])
