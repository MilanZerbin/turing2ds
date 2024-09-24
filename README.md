# Turingmachines to Dynamical Systems

This is the Git-Repository for a small program visualizing the encoding of Turing-Machines in Dynamical Systems.

## History

In ... , Moore showed in ...

## The Project

The code is separated into three main components:

- [ ] A library defining Turing Machines and visualizing them
- [ ] A library defining generalized shift dynamics and visualizing them
- [ ] A library defining PL-fold maps on the Unit Square and visualizing them

On top of this sits the main file, which upon execution guides the user to defining a Turing-Machine and providing an Input Band. Upon finishing, it will provide an Animation of said Turing Machine in all 4 described Versions.
 
## How to use

Clone the Repo:
```
git clone git@github.com:MilanZerbin/turing2ds.git
cd turing2ds
```
Install [Haskell](https://www.haskell.org/) and open a Haskell Shell such as [GHCi](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html) . Under NixOs for example, run 
```
nix-shell -p ghc --run ghci
```
Install dependencies, compile and Load the code
```
:l main.hs
```
Enjoy!


## Collaborators
The code is mainly written by [Milan Zerbin](https://milan.zerb.in), [Søren Dyhr](https://sites.google.com/upc.edu/complexfluids/who-are-we/s%C3%B8ren-istv%C3%A1n-adorj%C3%A1n-dyhr) and Jessica Ploner.

## License
This Project is licensed under [GNU GPL v3.0 License](https://www.gnu.org/licenses/gpl-3.0.en.html). You may freely distribute, copy and build upon the code, except for non free use.
