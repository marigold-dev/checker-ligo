# Checker

Pure CameLIGO version of the [Checker](https://github.com/tezos-checker/checker) project. Compiles
with Ligo 0.60 or newer.

This repository aims to propose an easier development experience than the original Checker
project:

* pure LIGO experience (including tests);
* fewer scripting languages required to compile and deploy;
* fewer vendored repositories (and, eventually, none). However, we're not there yet.

## Requirements

### Ligo
Compilation of Checker currently requires a small change in the LIGO compiler. Until this change is
ported to the compiler, it is required to install a custom version by following these steps:

* clone the following repository, including its submodules:
```
    git clone --recurse-submodules https://github.com/aguillon/ligo
```
* install the [opam](https://opam.ocaml.org/) package manager
* build LIGO (this can take some time):
```
    cd ligo
    make
```

Please refer to [the official repository](https://gitlab.com/ligolang/ligo/-/blob/dev/INSTALL.md)
for more detailed instructions.
* using an alias or equivalent, make sure the produced executable is in your path under the name
  `ligo`:
```
    alias ligo=$PathToLigoRepository/_build/default/src/bin/runligo.exe
```

### Python

Currently, the only way of building and deploying Checker easily is with Python. Other languages may
be available in the future.

* we recommend using [pyenv](https://realpython.com/intro-to-pyenv/) to manage your dependencies;
* make sure you have Python ≥ 3.9 and [pytezos](https://pytezos.org/) and that you create a specific
  environment;
* install the `checker` package using `python setup.py install`;
* optionally, install Jupyter to run the tutorial notebook.

### Submodules

We currently require 3 submodules: Ctez (for development purpose), Breathalyzer (to run tests) and
the math Cameligo library; make sure you clone the repository with the submodules.

## Compilation and deployment

The compilation script has been simplified and now only uses Python. You should be able to compile
the project using `checker_tools.client.compilation.compile_everything(path)`.

Please refer to the tutorial notebooks for more detailed instructions.
