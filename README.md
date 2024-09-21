# Turingmachines to Dynamical Systems

This is the Git-Repository for a small program visualizing the encoding of Turing-Machines in Dynamical Systems.

## History

In ... , Moore showed in ...

## The Project

The code is separated into four main components:

- [ ] A library defining Turing Machines and visualizing them
- [ ] A library defining generalized shift dynamics and visualizing them
- [ ] A library defining PL-fold maps on the Unit Square and visualizing them
- [ ] A library defining Dynamical Systems on the Disk and visualizing them

On top of this sits the main file, which upon execution guides the user to defining a Turing-Machine and providing an Input Band. Upon finishing, it will provide an Animation of said Turing Machine in all 4 described Versions.
 
## How to use

Clone the Repo:
```
git clone git@git.rz.uni-augsburg.de:zerbinmi/turing2ds.git
cd turing2ds
```
Open a Julia shell
```
nix-shell -p julia-bin --run julia
```
Compile and Load the code
```
:l main.jl
```
Enjoy!


## Collaborators
The code is mainly written by [Milan Zerbin](https://milan.zerb.in) and [Søren Dyhr](https://cab.bac.cat). We thank Jessica Plner for contributions.

## License
This Project is licensed under CC-BY-SA License. You may freely distribute, copy and build upon the code, except for commercial use.
