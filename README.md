# Dao: a Protocol for Production Rule Databases

#### Copyright (C) 2008-2017, Ramin Honary, all rights reserved.

Dao is a protocol in the Lisp/Scheme family of languages, which I call Dao Lisp.
Being a protocol, Dao Lisp is intended to be read and written by software, not
by human programmers, and as such Dao Lisp does not adhere to any Lisp or Scheme
standard (not compatible with Common Lisp, not compatible with R5RScheme). Dao
Lisp is designed specifically for modeling production rules to construct
databases of knowledge, what some call "knowledge bases," for the purpose of
simple, non-neural-network type artificial intelligence programs, with a focus
on natural language modeling and interaction.

Dao is also a Haskell library for generating production rule databases. This
library allows you to easily define your own Built-In Functions (BIFs). It is
intended that you write your whole program in Haskell, and the Haskell functions
Haskell functions you define can be called from the Dao Lisp runtime by calling
the BIFs you define.

To use Dao to construct a natural language understanding program (e.g. a
program that understands plain English), you need to map the words or syllables
of your natural language to Dao Lisp atoms or strings (there are other
primitive types besides atoms and strings, including integers, dictionaries,
lists, and executable functions). You then define patterns, which are somewhat
similar to UNIX glob patterns, and these patterns will match against queries
which are simply sequences of atoms and strings. For patterns that match,
actions (Lisp expressions) are executed in response, although these Lisp
expressions are simply Lisp function calls that call back into your Haskell
program via the BIFs you define.

Pattern-Action pairs are called production rules (or just "Rules"), and can be
stored into a flat file database which we call a "knowledge base." It is fairly
simple to construct multiple knowledge bases and merge them together at run
time.

Begin by reading the comments of the "src/Language/Interpreter/Dao/GHCI.hs"
module, which provides useful functions for constructing a production rule
database and testing it with simple queries. There are also functions for
tracing query execution, seeing which production rule patterns match the query,
and for tracing the function execution that construct data, or throw errors, in
response to a query.

--------------------------------------------------------------------------------

Dao has change quite a lot over the years. Originally a graduate student
project, the goals and target features of the project have been redefined again
and again since it was first released on Hackage. The most consistent goal of
the project has always been to allow programmers to build production-rule-based
systems for understanding natural language. The production rule syntax, the data
structures comprising the database, and the rule execution model have never been
consistent between versions, however since it has not been anything more than an
experimental project with a tiny user base, a complete re-write of the
interpreter and language has not been a problem.

Dao is licensed under the GNU Affero General Public License:
	http://www.gnu.org/licenses/agpl.html

### Related Haskell packages

This project depends on some other packages I have written, including
`random-walks` and `token-parser`, both of which are published on
<https://hackage.haskell.org>.

The <https://github.com/RaminHAL9001/Dao-examples> project on GitHub contains
some sample databases which I have constructed using Dao. This project also
contains an `ncurses`-based Read-Eval-Print Loop (REPL) which can be built in
any Haskell platform that can import the `readline` package, (usually
UNIX/Linux systems only).

