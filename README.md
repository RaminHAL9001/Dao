# The Dao Programming Language
Copyright (C) 2008-2015, Ramin Honary, all rights reserved.

## A pure Haskell dynamically typed DSL for production-rule-based logic programming and AI.

Dao is a Haskell package (library), designed for constructing simple artificial
intelligence programs that can understand natural (human) languages, and is
licensed under the GNU Affero General Public License:
	http://www.gnu.org/licenses/agpl.html

Dao is a dynamically typed Domain Specific Language (DSL) programmable with the
Haskell programming language. Dao is intended providing tools for logic
programming and constructing knowledge bases, with a focus on artificial
intelligence.  Reminiscent of the PROLOG programming language, but made better
with Haskell's static type checking and clean semantics, Dao is well suited for
things like type checking, computing build dependencies, whole-program
analysis, rapid prototyping of parsers, and Read-Eval-Print Loop-based
(REPL-based) source code development, as well as natural language
comprehension.

Dao originally defined a JavaScript-like programming language for defining
production rule databases suited to natural language understanding, but now it
is just a DSL with tools for defining grammars and parsers, where all
production rules are defined in a Haskell program using the Dao APIs. I may
bring back "Dao-script" if I see a need for it in the future, or if there is
any demand for it.

The more long-term goal of Dao is to create a laboratory with a rich set of
tools for experimenting with artificial intelligence, especially for
understanding natural human language. The Dao system aspires to become the
testing ground for a new wave of effective, practical, natural language user
interfaces.

For a tutorial on how to use Dao, please refer to the "Dao-examples" package
(soon to be released) on Git Hub and Hackage, which contains many
well-documented examples making use of the Dao (DSL) for solving problems with
a knowledge base, and for developing rudimentary artificial intelligence.

## History
The Dao System is the result of my masters thesis, "Natural Language
Understanding Systems using the Dao Programming Environment" published
at the Tokyo Institute of Technology in 2007. The first public release
of the Dao System was made available at <http://hackage.haskell.org> in
March of 2008, although it was mostly incomplete. The latest code is now
available at <https://github.com> . Releases will be made available at
Hackage as further progress is made.

