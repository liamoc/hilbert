hilbert
=======

Hilbert is a theorem prover designed for people who don't want to learn a theorem prover, specifically students studying formal treatments of programming languages.

It's designed to follow as closely as possible the standard natural deduction they do by hand, rather than introduce its own sophisticated meta-logic. It features a graphical interface that renders "proof trees" as stacked rules as is the notational convention, using unicode and `vty`. Proofs are represented as such trees. There is no tactic script.

Currently, the meta-logic is not even powerful enough to express full natural deduction - specifically the notion of implication cannot be indirectly captured in the usual fashion. Instead, it's based entirely on string-based formal systems such as those found in GEB or similar. 

This makes it ideal for exploring basic string judgements, but not much good for anything beyond that just yet.

This version can be considered a prototype for the interface of the next version, Hilbert 2, which will include user-definable, tree-based syntax; unification (to allow free schematic variables and working in either direction); and implications (to allow more complicated logics). It will still be a first-order logic though, so for induction I'll have to provide some built-in primitives.

This version has a few nice features though: It allows judgements to take any form, because everything is just strings internally, but this also means you have to proceed in a strictly bottom-up fashion, allowing no schematic variables to remain unresolved. 

Building
========

Hilbert can be built on GHC 7.6 with `vty` and `vty-ui` installed along with the usual Haskell libraries, as well as the `split` package. Just use `cabal configure` and `cabal build` to give it a try.


Using
=====

Run hilbert as:

    hilbert [files]

Each of the files should contain a list of hilbert rules. An example bracket matching language is included in the repository. Essentially:

 - 1 or more blank lines separate each rule
 - Rules are written as follows:


               string schema
          -------------------- NAME
               string schema


Where string schema are any sequence of characters, which may contain variables, which are notated with `?x` (variable names must be one character long). For example, the unary natural numbers can be defined by:


      ---------- ZERO
           Nat
      
        ?x Nat
      ----------- SUC
        ?x| Nat

When hilbert opens, you will be prompted for a proof goal. This should be a concrete sentence (no variables!). Then you will be in the prover interface for your goal. The controls are:

 - `r` - Apply a rule to the selected goal
 - `w` - Move to a subgoal of the selected goal, or confirm a choice in the rule application menu.
 - `s` - Move back to the parent goal of the selected subgoal, or cancel a rule application menu.
 - `a` - Move to the previous sibling of the current goal, or move to the previous possible rule application if in the menu.
 - `d` - Move to the next sibling of the current goal, or move to the next possible rule application if in the menu.
 - `q` - If in the rule application menu, go to the previous interpretation of the current rule (this is for ambiguous rules)
 - `e` - If in the rule application menu, go to the next interpretation of the current rule
 - `z` - Clear the current rule application on the selected goal and all it's children.
 - `f` - Instantiate any free schematic variables in subgoals of the selected goal.
 - `x` - Exit

Have fun!

Disclaimer
----------
I'm aware this is painfully impractical at this point. Mostly it's a prototype for the UI, not the theorem prover. The actual theorem prover will be in version 2.