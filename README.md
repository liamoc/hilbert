hilbert
=======

Hilbert is a theorem prover designed for people who don't want to learn a theorem prover, specifically students studying formal treatments of programming languages.

It's designed to follow as closely as possible the standard natural deduction they do by hand, rather than introduce its own sophisticated meta-logic. It features a graphical interface that renders "proof trees" as stacked rules as is the notational convention, using unicode and `vty`. Proofs are represented as such trees. There is no tactic script.

This branch of hilbert supports a first-order term language, which is sufficient to encode a lot of useful systems, mostly of the propositional variety. Some type systems can also be specified in it, but the absence of substitution means that it is mostly of educational benefit.

It features a fairly flexible syntax and supports full natural deduction including hypothetical derivations and locally-bounded hypothetical variables; all presented in Gentzen-style proof trees. It also supports schematic (unification) variables to be littered throughout the proof tree, allowing you to run through a set of rules as a computation and see what can be deduced about the output.

Examples, including full propositional logic, are provided in the examples directory.

Building
========

Hilbert can be built on GHC 7.6 with `vty` and `vty-ui` installed along with the usual Haskell libraries, as well as the `split` package. Just use `cabal configure` and `cabal build` to give it a try.


Using
=====

Run hilbert as:

    hilbert [files]

Each of the files should contain a list of hilbert rules. An example propositional logic is included, which
demonstrates all the features.

When hilbert opens, you will be prompted for a proof goal. This should be a simple logical sentence, which may include schematic variables (prefixed with '?'). Then you will be in the prover interface for your goal. The controls are:

 - `r` - Apply a rule to the selected goal
 - `w` - Move to a subgoal of the selected goal, or confirm a choice in the rule application menu.
 - `s` - Move back to the parent goal of the selected subgoal, or cancel a rule application menu.
 - `a` - Move to the previous sibling of the current goal, or move to the previous possible rule application if in the menu.
 - `d` - Move to the next sibling of the current goal, or move to the next possible rule application if in the menu.
 - `z` - Clear the current rule application on the selected goal and all it's children.
 - `f` - Instantiate any free schematic variables in subgoals of the selected goal.
 - `x` - Exit

Have fun!

