# The Dao Programming Language
Copyright (C) 2008-2017, Ramin Honary, all rights reserved.

### A pure Haskell DSL for production-rule-based logic programming and AI.

Dao is a Haskell package providing an Embedded Domain Specific
Language (EDSL) designed for constructing simple knowledge bases,
artificial intelligence programs, and programs for understanding
natural language.

Dao is licensed under the GNU Affero General Public License:
	http://www.gnu.org/licenses/agpl.html

Dao is a Domain Specific Language (DSL) intended providing tools for logic
programming and constructing knowledge bases, with a focus on artificial
intelligence. It is reminiscient of the PROLOG programming language, but made
better with Haskell's static type checking and clean semantics.

In this latest version, Dao provides a library of tools for constructing a
database of predicates, where each predicate is a UNIX-like "glob" pattern
(e.g. where the `\*` character indicates a wildcard). The database is made
persistent as a flat file of S-expressions (the syntax of the Lisp and Scheme
programming language). There are combinators for evaluating queries against a
rule database, but the implementation of the actual query execution, which
involves sanitizing and tokenizing an input query string into a list of
pattern-matchable tokens, is a detail left to the programmer (you) who choses
to import the Dao package into their project.

The `Dao.GHCI` module is also provided, which defines a set of commands that
can be imported into a GHCi session. With these commands at your disposal in
GHCi, you will be able to construct rule database from a corpus of text, load
previously constructed databases, and experiment with query execution.

Dao has change quite a lot over the years. Originally a graduate student
project, the goals and target features of the project have been redefined again
and again since it was first released on Hackage. The most consistent goal of
the project has always been to allow programmers to build production-rule-based
systems for understanding natural language. The production rule syntax, the
data structures comprising the database, and the rule execution model have
never been consistent between versions.

### Related Haskell packages

This project depends on some other packages I have written, including
`random-walks` and `token-parser`, both of which are published on
<https://hackage.haskell.org>.

The <https://github.com/RaminHAL9001/Dao-examples> project on GitHub contains
some sample databases which I have constructed using Dao. This project also
contains an `ncurses`-based Read-Eval-Print Loop (REPL) which can be built in
any Haskell platform that can import the `readline` package, (usually
UNIX/Linux systems only).

