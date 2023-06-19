# Checker

Pure CameLIGO version of the [Checker](https://github.com/tezos-checker/checker) project. Compiles
with Ligo 0.67.1 or newer.

This repository aims to propose an easier development experience than the original Checker
project:

* pure LIGO experience (including tests);
* fewer scripting languages required to compile and deploy;
* fewer vendored repositories (and, eventually, none).

While some of these goals have not been met yet, we encourage you to deploy the system on Ghostnet
and explore it through the Jupyter notebooks available in the `tutorials/` directory.

## Online demo version

A demo version is available online: you can execute [the first tutorial notebook thanks to ![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/marigold-dev/checker-ligo/HEAD?labpath=tutorials%2F01-testing-checker.ipynb).

## Requirements

We strongly encourage you to build the project locally before using it in production.

### Ligo

Please refer to [the official LIGO repository](https://gitlab.com/ligolang/ligo/-/blob/dev/INSTALL.md)
for installation instructions. The current build script assumes that a `ligo` executable is
available in your `$PATH`. For instance, if you compiled LIGO from source, you can place the following
script in your `$PATH`:
```
#!/bin/env sh

/path/to/ligo/directory/_build/default/src/bin/runligo.exe "$@"
```

### Python

Currently, the only way of building and deploying Checker easily is with Python. Other languages may
be available in the future.

* we recommend using [pyenv](https://realpython.com/intro-to-pyenv/) to manage your dependencies;
* make sure you have Python ≥ 3.9 and [pytezos](https://pytezos.org/) and that you create a specific
  environment;
* install the `checker` package using `pip install .` in the main directory;
* optionally, install Jupyter to run the tutorial notebook.

### Submodules

We currently require 3 submodules: Ctez (for development purpose), Breathalyzer (to run tests) and
the math Cameligo library; make sure you clone the repository with the submodules.

## Compilation and deployment

The compilation script has been simplified and now only uses Python. You should be able to compile
the project using `checker_tools.client.compilation.compile_everything(path)`.

Please refer to the tutorial notebooks for more detailed instructions.
