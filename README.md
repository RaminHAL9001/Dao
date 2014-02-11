# The Dao Programming Language
Copyright (C) 2008-2014, Ramin Honary, all rights reserved.

**NOTICE:** This programming language is HIGHLY EXPERIMENTAL, and many major
changes are being made to the language definition, the interpreter, the
supporting libraries and functions, and it is all changing at a very rapid
rate.

The Dao System is a Haskell package (library), and an interactive
command line interface to this library, designed for constructing simple
artificial intelligence programs that can understand natural (human)
languages, and is licensed under the GNU Affero General Public License:
	http://www.gnu.org/licenses/agpl.html

In short, the Dao programming language is a fusion between Prolog and
JavaScript. Those who have tried to use Prolog coming from a procedural
programming background quickly get frustrated trying to figure out why
the language behaves the way it does, for example where to place cuts
(the ! operator) to prevent excessive backtracking and to prevent the
program from doing things twice. Prolog has it's uses, but it is
difficult to use correctly.

I always wished for a programming language with the logical solving
power of Prolog but with the straight-forward execution model of a
C-like programming language. This wish led me to invent the invention
Dao programming language.

Dao could have been simply a domain-specific language implemented in
Haskell, after all using lists as monads is an excellent way to emulate
the behavior of a Prolog solver. But I decided to build it up to a
full-fledged programming language for a few reasons which I will
summarize here. The following three principles have guided my
implementation of the Dao language and interpreter:

1.	Dao is a language to build ontologies, define axioms on those
ontologies, and provides a means to map natural human language patterns
to these ontologies and the related axioms. Dao is much more concise
than XML which should make it easy to quickly write words, phrases, and
grammar as patterns, and describe the semantics of these constructs
using the Dao language. Axioms are expressed as functions (similar to
JavaScript functions) and can describe how to transform ontological data
structures to perform logic and reasoning. In a way, Dao is here to let
you write a dictionary, where the definition of words are written as a
data structures and executable script code.

2.	Dao is a scripting language extension for Haskell applications,
analogous to what Lua is for C/C++. Dao is NOT a general purpose
language. Dao is not intended to be used for the development of APIs or
applications. You write APIs and applications in Haskell. You extend
your application with the modules in the Dao package. You extend Dao's
API with Haskell, and use Dao's foreign function interface to execute
Haskell code. Programmers can script portions of their applications in
Dao. End users can use Dao's natural language features to issue
instructions to your application, or to construct their own scripts in
an intuitive way using language that anyone understands.

3.	Dao is a laboratory containing a rich set of tools for experimenting
with artificial intelligence, with a focus on computers understanding
natural human language. The Dao system aspires to become the testing
ground for a new wave of effective, practical, natural language user
interfaces.

## How it works
The Dao scripting language is designed to be similar to languages like
JavaScript which is probably the most popular language in common use
today (at the time of this writing). It is also similar to AWK, S-Lang,
and Lua. However there are a few novelties that make Dao a bit
different.

The Dao runtime is vaguely similar to the UNIX "AWK" language. However,
the Dao language provides a much more feature-rich set of built-in data
types and functionality as compared to AWK, notably the ability to
execute rules recursively in the same process. The Dao language makes
use of patterns called "globs" rather than POSIX regular expressions.
These glob patterns are so-called because they are inspired by UNIX
"glob" expressions, but in Dao they are more suitable for matching
against natural language input. Glob patterns are optimized for faster
matching of input text.

When an input string is matched against all of the glob patterns in a
Dao program, that input string is referred to as a "query." All patterns
that match the query will execute their associated subroutine which is
called an "action." I refer to this procedure as "executing the input
string query against the program" or just "executing the input string."

Dao programs may import other programs as companions and execute input
queries against companion programs, as well as recursively executing
queries against itself. Imported companion programs are referred to as
"modules." Each module runs in it's own thread so executing of string
queries is inherently a parallel computation if there are multiple
modules loaded.

As the Dao program executes input queries, the state of the program's
working memory is changed. The working memory is similar to an
HTML/JavaScript DOM tree, but there are many more primitive types than
what JavaScript provides. This tree can be serialized and stored to the
filesystem of the host computer, and re-loaded back into memory at any
time, although Dao's own built-in binary serialization format is used
for this, not JSON. These document files are informally called "idea"
files because they allow the Dao system to store knowledge, and
transmit it to other Dao installations.

### Dao Foreign Functions
The Dao foreign function interface provides to a Haskell programmer a
method to install your own functions into a running Dao language
interpreter. Classes similar to lenses are provided to instantiate
functions that can convert Haskell data types to and from the data
structures that can be manipulated by Dao language expressions.

The Dao language interpreter borrows from the C language the concept of a
`struct`, although internally, a Dao struct is actually a tree, and the Dao
language has a similar syntax for reading or writing to fields of these
structs, which is similar to the syntax of how JavaScript modifies the
DOM tree:
```c
	person.home.email = "first.last@mail.com";
```

### Artificial Intelligence?
How this all relates to artificial intelligence is that the working
memory of a Dao program forms an ontology. Functions can be defined to
manipulate the working memory in object-oriented fashion. The patterns
that can match input queries formulate the axioms of the system. When an
input query is executed, the Dao system can use these axioms to perform
logical reasoning on the ontological objects in its working memory.

## A simple example, Dao compared to AWK

To be clear, the purpose of Dao is completely different than the purpose
of AWK. However, the Dao runtime uses a similar pattern matching and
execution algorithm to that of AWK, so comparing Dao to AWK can be
instructive.

Observe the following AWK program:
```awk
	# example.awk
	/^ *put.*in/ {
		match($0, /^put *(.*) *in *(.*) *$/, matched);
		what_to_put     = matched[1];
		print("> what to put: " what_to_put);
		where_to_put_it = matched[2];
		print("> where to put it: " where_to_put_it);
	}
	/^ *my *name *is/ {
		match($0, /^ *my *name *is *(.*) *$/, matched);
		user_name = matched[1];
		print("> Hello, " user_name "!");
	}
```

You can then interact with the AWK program like so:
```console
	% awk -f example.awk
	my name is Dave
	> Hello, Dave!
	put the data into the spreadsheet
	> what to put: the data
	> where to put it: the spreadsheet
```

AWK was not designed with natural language in mind, so it is ill-suited
to natural language systems:
* POSIX regular expressions are not designed to match
natural language input.
    1.	You need to place a Kleene-star after every space, and and
	capture "wildcards" in parentheses.
    2.	Typing mistakes, like writing "naem" instead of "name" would
	fail to behave as expected.
* It is necessary to build the "matched" array with a separate call to
the "match()" built-in function.
* Modules cannot be imported and there is no concurrency.
* All variables are global.
* There is no facility to serialize the state of the program and store
it to, or reload it from, the file system.
* There is no function to force a string into the standard input, so
recursive pattern matching is not possible.

The Dao language and interpreter is designed for natural language
understanding, and addresses all of the above mentioned shortcomings.
The same program written in the Dao language would look like this:
```
	rule "put $what in $where" {
		what.to.put = what;
		println("> what to put: ", what);
		where.to.put = where;
		println("> where to put it: ", where);
	}
	rule "my name is $name" {
		user.name = name;
		println("> Hello, ", name);
	}
```

Dao's language is a bit more concise, both for patterns and for the
scripted actions. Dao provides ways to make pattern matching more
permissive, so that typing mistakes may also match the patterns. The
matching algorithm can be specified within the program. Future
implementations may allow for a pattern matching algorithm to take into
account contextual clues to make more accurate guesses on how to rectify
typing mistakes, and to facilitate gathering of statistical information
on input strings to better predict what an end user will type.

## History
The Dao System is the result of my masters thesis, "Natural Language
Understanding Systems using the Dao Programming Environment" published
at the Tokyo Institute of Technology in 2007. The first public release
of the Dao System was made available at <http://hackage.haskell.org> in
March of 2008, although it was mostly incomplete. The latest code is now
available at <https://github.com> . Releases will be made available at
Hackage as further progress is made.

