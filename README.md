# Checker

Pure CameLIGO version of the [Checker](https://github.com/tezos-checker/checker) project.

## Compilation

Unlike the original project, this repository does not vendor anything but the
[Breathalyzer](https://github.com/marigold-dev/breathalyzer) library.

Requirements (WIP):
- Python ≥ 3.9 and [pytezos](https://pytezos.org/)
- Ligo ≥ 0.58, available in your `$PATH` under the name `ligo`

The compilation script has been simplified and now only uses Python. You should be able to compile
the project using `checker_tools.client.compilation.compile_everything(path)`. If not, I apologize.

## Deployment

TBD
