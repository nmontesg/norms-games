# A Computational Model of the IAD framework

This repository implements a computational model of Elinor Ostrom's
Institutional Analysis and Development framework. It includes the interpreter to
the Action Situation Language (ASL) and the game engine to automatically
generate extensive-form games from ASL descriptions.

## Requirements

_norms-games_ requires a working installation of the following:
* Python 3
* [SWI-Prolog](https://www.swi-prolog.org/)
* The [PySwip](https://github.com/yuce/pyswip) package

## Usage

To install the ``norms-games`` package, first clone the repository in your local
file system:

    git clone https://github.com/nmontesg/norms-games.git

Navigate to the root of the ``norms-games`` repository and install the package
in editable mode using pip, from the environment you are using for your project:

    cd /your/local/path/ngames
    pip install --editable .

For the time being, the ``norms-games`` package requires to download a local
copy of the source code (using ``git clone``). The path to the package should
then [be appended to your Python
path](https://www.johnny-lin.com/cdat_tips/tips_pylang/path.html).

The ``examples`` directories has some illustrations on how to use the basic
functions. Basically, you should create your ASL description in three distinct
files:

- ``agents.pl``
- ``states.pl``
- ``rules.pl``

Then, to construct the extensive-form game semantics of your description, it is
enough to call:

    build_full_game(<path_to_ASL_description>, <identifier>, (threshold=..., max_rounds=...))

See the documentation for further details.


### References

Ostrom, E. (2005). Understanding Institutional Diversity. Princeton University
Press.

Montes, N., Osman, N., & Sierra, C. (2021). Enabling Game-Theoretical Analysis
of Social Rules. In _Artificial Intelligence Research and Development_ (Vol.
339, pp. 90–99). IOS Press. https://doi.org/10.3233/FAIA210120

Montes, N., Osman, N., & Sierra, C. (2022). A Computational Model of Ostrom’s
Institutional Analysis and Development Framework. _Artificial Intelligence_
(Vol. 311). Elsevier. https://doi.org/10.1016/j.artint.2022.103756
