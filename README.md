# vim with training wheels

## Description

This project is part of a bachelorthesis with the goal to help understanding modal editors.

It's main features are a text editor with basic vim functionality and a tutorial.

It is built using the Haskell.Brick TUI library.
Using Colors and Helptexts to guide users through their first vim experience.

The tutorial lays focus on the systematic behind the commands and the modality.


## Overview over the haskell modules

[Modules graph](./Documentation/modules.pdf)

### generating new overview graph
```bash
stack exec graphmod | tred | dot -Tpdf > modules.pdf
```

## Installation

The project uses [stack](https://docs.haskellstack.org/en/stable/) to build and install the program.

```
stack install
```
## Test

```
stack test
```

## Usage
To run the program use

```
vim-tw
```
it will open the landing page where you can choose between the tutorial and opening a file with the editor.


## License

[GNU GPLv3](https://choosealicense.com/licenses/gpl-3.0/)
