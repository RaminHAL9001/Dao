# "ghc-build-opts.mk": a GNU-Make script to build the "Dao" modules
# and interactive program.
# 
# Copyright (C) 2008-2013  Ramin Honary.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program (see the file called "LICENSE"). If not, see
# <http://www.gnu.org/licenses/agpl.html>.
####################################################################################################

.PHONEY: all test clean update edit info

shell:=bash
slash:=/
dot:=.
star:=*
hash:=\#

include ghc-build-opts.mk

ifdef BUILTIN_RTS_OPTIONS
RTS_OPTS := -with-rtsopts="$(BUILTIN_RTS_OPTIONS)"
endif

ifeq ($(ALLOW_CHANGE_RTS_OPTIONS),true)
RTS_OPTS += -rtsopts
endif

SRC_DIRS = $(foreach i,$(SOURCE_DIRECTORIES),-i'$i')
LANG_EXTS = $(foreach X,$(LANGUAGE_EXTENSIONS),-X$X)
GHC_CMD = ghc
GHC_BUILD = $(GHC_CMD) $(SRC_DIRS) $(GHC_FLAGS) $(RTS_OPTS) $(LINKER_FLAGS) $(LANG_EXTS)
GHC_GEN_DEPS = $(GHC_CMD) $(SRC_DIRS) -M

####################################################################################################

all: dao test

dao: src/dao-main.o
	$(GHC_BUILD) -o dao src/dao-main.hs $(LINKER_FLAGS)

test: debug debug/test

debug:
	mkdir -p debug

debug/test: debug tests/Dao/Test.o
	$(GHC_BUILD) -o debug/test tests/main.hs

debug/single: debug debug/single.hs
	$(GHC_BUILD) -o debug/single debug/single.hs

clean:
	find ./src ./tests \( -name '*.o' -o -name '*.hi' \) -printf 'rm %p;\n' -delete;

# Updates this Makefile using ghc -M to create every target at the bottom of this file.
update:
	$(GHC_GEN_DEPS) src/dao-main.hs tests/main.hs && rm -f Makefile.bak

listfile = grep -v '^[[:space:]]*$(hash).*$$' $1

edit: $(EDIT_FILES_LIST)
	vim -p$(NUMBER_OF_TABS) \
		$(EDIT_FILES_LIST) \
		$(shell $(call listfile,$(EDIT_FILES_LIST)))

NOTE := $(hash)$(hash)$(hash)

info:
	@echo '$(NOTE) GHC Build command is:'
	@echo '$(NOTE) $(GHC_BUILD)'

%.hi: *.hs
	@echo GHC: $*.hs && $(GHC_BUILD) $*.hs

####################################################################################################

# DO NOT DELETE: Beginning of Haskell dependencies
src/Dao/EnumSet.o : src/Dao/EnumSet.hs
src/Dao/String.o : src/Dao/String.hs
src/Dao/Procedural.o : src/Dao/Procedural.hs
src/Dao/Predicate.o : src/Dao/Predicate.hs
src/Dao/Predicate.o : src/Dao/String.hi
src/Dao/Tree.o : src/Dao/Tree.hs
src/Dao/Glob.o : src/Dao/Glob.hs
src/Dao/Glob.o : src/Dao/Tree.hi
src/Dao/Glob.o : src/Dao/String.hi
src/Dao/PPrint.o : src/Dao/PPrint.hs
src/Dao/PPrint.o : src/Dao/String.hi
src/Dao/Token.o : src/Dao/Token.hs
src/Dao/Token.o : src/Dao/String.hi
src/Dao/Parser.o : src/Dao/Parser.hs
src/Dao/Parser.o : src/Dao/EnumSet.hi
src/Dao/Parser.o : src/Dao/Predicate.hi
src/Dao/Parser.o : src/Dao/Token.hi
src/Dao/Parser.o : src/Dao/String.hi
src/Dao/Stack.o : src/Dao/Stack.hs
src/Dao/Stack.o : src/Dao/Tree.hi
src/Dao/Runtime.o : src/Dao/Runtime.hs
src/Dao/Runtime.o : src/Dao/String.hi
src/Dao/Struct.o : src/Dao/Struct.hs
src/Dao/Struct.o : src/Dao/Predicate.hi
src/Dao/Struct.o : src/Dao/Tree.hi
src/Dao/Struct.o : src/Dao/String.hi
src/Dao/Struct.o : src/Dao/Runtime.hi
src/Dao/Binary.o : src/Dao/Binary.hs
src/Dao/Binary.o : src/Dao/Tree.hi
src/Dao/Binary.o : src/Dao/String.hi
src/Dao/Binary.o : src/Dao/Runtime.hi
src/Dao/Object.o : src/Dao/Object.hs
src/Dao/Object.o : src/Dao/Procedural.hi
src/Dao/Object.o : src/Dao/Predicate.hi
src/Dao/Object.o : src/Dao/Struct.hi
src/Dao/Object.o : src/Dao/Stack.hi
src/Dao/Object.o : src/Dao/Binary.hi
src/Dao/Object.o : src/Dao/Tree.hi
src/Dao/Object.o : src/Dao/EnumSet.hi
src/Dao/Object.o : src/Dao/Glob.hi
src/Dao/Object.o : src/Dao/Parser.hi
src/Dao/Object.o : src/Dao/Token.hi
src/Dao/Object.o : src/Dao/String.hi
src/Dao/Object.o : src/Dao/Runtime.hi
src/Dao/Object/AST.o : src/Dao/Object/AST.hs
src/Dao/Object/AST.o : src/Dao/Object.hi
src/Dao/Object/AST.o : src/Dao/Parser.hi
src/Dao/Object/AST.o : src/Dao/Token.hi
src/Dao/Object/AST.o : src/Dao/String.hi
src/Dao/Object/DeepSeq.o : src/Dao/Object/DeepSeq.hs
src/Dao/Object/DeepSeq.o : src/Dao/Tree.hi
src/Dao/Object/DeepSeq.o : src/Dao/Parser.hi
src/Dao/Object/DeepSeq.o : src/Dao/Token.hi
src/Dao/Object/DeepSeq.o : src/Dao/EnumSet.hi
src/Dao/Object/DeepSeq.o : src/Dao/Glob.hi
src/Dao/Object/DeepSeq.o : src/Dao/Token.hi
src/Dao/Object/DeepSeq.o : src/Dao/Object/AST.hi
src/Dao/Object/DeepSeq.o : src/Dao/Object.hi
src/Dao/Resource.o : src/Dao/Resource.hs
src/Dao/Resource.o : src/Dao/Tree.hi
src/Dao/Resource.o : src/Dao/Procedural.hi
src/Dao/Resource.o : src/Dao/Object.hi
src/Dao/Resource.o : src/Dao/String.hi
src/Dao/Object/Math.o : src/Dao/Object/Math.hs
src/Dao/Object/Math.o : src/Dao/Tree.hi
src/Dao/Object/Math.o : src/Dao/Predicate.hi
src/Dao/Object/Math.o : src/Dao/Glob.hi
src/Dao/Object/Math.o : src/Dao/Object.hi
src/Dao/Object/PPrint.o : src/Dao/Object/PPrint.hs
src/Dao/Object/PPrint.o : src/Dao/Tree.hi
src/Dao/Object/PPrint.o : src/Dao/EnumSet.hi
src/Dao/Object/PPrint.o : src/Dao/Glob.hi
src/Dao/Object/PPrint.o : src/Dao/Object/AST.hi
src/Dao/Object/PPrint.o : src/Dao/Object.hi
src/Dao/Object/PPrint.o : src/Dao/Token.hi
src/Dao/Object/PPrint.o : src/Dao/PPrint.hi
src/Dao/Object/PPrint.o : src/Dao/String.hi
src/Dao/Object/Struct.o : src/Dao/Object/Struct.hs
src/Dao/Object/Struct.o : src/Dao/Tree.hi
src/Dao/Object/Struct.o : src/Dao/Token.hi
src/Dao/Object/Struct.o : src/Dao/EnumSet.hi
src/Dao/Object/Struct.o : src/Dao/Glob.hi
src/Dao/Object/Struct.o : src/Dao/Parser.hi
src/Dao/Object/Struct.o : src/Dao/Predicate.hi
src/Dao/Object/Struct.o : src/Dao/Struct.hi
src/Dao/Object/Struct.o : src/Dao/Object/AST.hi
src/Dao/Object/Struct.o : src/Dao/Object.hi
src/Dao/Object/Parser.o : src/Dao/Object/Parser.hs
src/Dao/Object/Parser.o : src/Dao/EnumSet.hi
src/Dao/Object/Parser.o : src/Dao/Tree.hi
src/Dao/Object/Parser.o : src/Dao/Parser.hi
src/Dao/Object/Parser.o : src/Dao/Predicate.hi
src/Dao/Object/Parser.o : src/Dao/Object/AST.hi
src/Dao/Object/Parser.o : src/Dao/Object.hi
src/Dao/Object/Parser.o : src/Dao/Object/PPrint.hi
src/Dao/Object/Parser.o : src/Dao/PPrint.hi
src/Dao/Object/Parser.o : src/Dao/Token.hi
src/Dao/Object/Parser.o : src/Dao/String.hi
src/Dao/Random.o : src/Dao/Random.hs
src/Dao/Random.o : src/Dao/Tree.hi
src/Dao/Random.o : src/Dao/Object/AST.hi
src/Dao/Random.o : src/Dao/Object.hi
src/Dao/Random.o : src/Dao/Glob.hi
src/Dao/Random.o : src/Dao/Token.hi
src/Dao/Object/Random.o : src/Dao/Object/Random.hs
src/Dao/Object/Random.o : src/Dao/Tree.hi
src/Dao/Object/Random.o : src/Dao/Object/AST.hi
src/Dao/Object/Random.o : src/Dao/Random.hi
src/Dao/Object/Random.o : src/Dao/Object.hi
src/Dao/Object/Random.o : src/Dao/Glob.hi
src/Dao/Object/Random.o : src/Dao/Token.hi
src/Dao/Object/Binary.o : src/Dao/Object/Binary.hs
src/Dao/Object/Binary.o : src/Dao/Binary.hi
src/Dao/Object/Binary.o : src/Dao/EnumSet.hi
src/Dao/Object/Binary.o : src/Dao/Glob.hi
src/Dao/Object/Binary.o : src/Dao/Tree.hi
src/Dao/Object/Binary.o : src/Dao/Object.hi
src/Dao/Object/Binary.o : src/Dao/Token.hi
src/Dao/Prelude.o : src/Dao/Prelude.hs
src/Dao/Prelude.o : src/Dao/Object/Binary.hi
src/Dao/Prelude.o : src/Dao/Object/Random.hi
src/Dao/Prelude.o : src/Dao/Object/Struct.hi
src/Dao/Prelude.o : src/Dao/Object/Parser.hi
src/Dao/Prelude.o : src/Dao/Object/AST.hi
src/Dao/Prelude.o : src/Dao/Object/PPrint.hi
src/Dao/Prelude.o : src/Dao/Binary.hi
src/Dao/Prelude.o : src/Dao/Random.hi
src/Dao/Prelude.o : src/Dao/Parser.hi
src/Dao/Prelude.o : src/Dao/Token.hi
src/Dao/Prelude.o : src/Dao/Predicate.hi
src/Dao/Prelude.o : src/Dao/Struct.hi
src/Dao/Prelude.o : src/Dao/PPrint.hi
src/Dao/Prelude.o : src/Dao/Object.hi
src/Dao/Prelude.o : src/Dao/Tree.hi
src/Dao/Prelude.o : src/Dao/String.hi
tests/Dao/Test.o : tests/Dao/Test.hs
tests/Dao/Test.o : src/Dao/Object/DeepSeq.hi
tests/Dao/Test.o : src/Dao/Object/Random.hi
tests/Dao/Test.o : src/Dao/Object/Struct.hi
tests/Dao/Test.o : src/Dao/Object/PPrint.hi
tests/Dao/Test.o : src/Dao/Object/Binary.hi
tests/Dao/Test.o : src/Dao/Object/Parser.hi
tests/Dao/Test.o : src/Dao/Object/AST.hi
tests/Dao/Test.o : src/Dao/Object.hi
tests/Dao/Test.o : src/Dao/Binary.hi
tests/Dao/Test.o : src/Dao/Random.hi
tests/Dao/Test.o : src/Dao/Struct.hi
tests/Dao/Test.o : src/Dao/Parser.hi
tests/Dao/Test.o : src/Dao/PPrint.hi
tests/Dao/Test.o : src/Dao/Predicate.hi
tests/Dao/Test.o : src/Dao/String.hi
tests/Dao/Test.o : src/Dao/Prelude.hi
src/Dao/Evaluator.o : src/Dao/Evaluator.hs
src/Dao/Evaluator.o : src/Dao/Object/Parser.hi
src/Dao/Evaluator.o : src/Dao/Object/Struct.hi
src/Dao/Evaluator.o : src/Dao/Object/Binary.hi
src/Dao/Evaluator.o : src/Dao/Object/PPrint.hi
src/Dao/Evaluator.o : src/Dao/Object/Math.hi
src/Dao/Evaluator.o : src/Dao/Struct.hi
src/Dao/Evaluator.o : src/Dao/Resource.hi
src/Dao/Evaluator.o : src/Dao/Procedural.hi
src/Dao/Evaluator.o : src/Dao/Predicate.hi
src/Dao/Evaluator.o : src/Dao/Glob.hi
src/Dao/Evaluator.o : src/Dao/Tree.hi
src/Dao/Evaluator.o : src/Dao/PPrint.hi
src/Dao/Evaluator.o : src/Dao/Object/DeepSeq.hi
src/Dao/Evaluator.o : src/Dao/Object/AST.hi
src/Dao/Evaluator.o : src/Dao/Object.hi
src/Dao/Evaluator.o : src/Dao/Parser.hi
src/Dao/Evaluator.o : src/Dao/Token.hi
src/Dao/Evaluator.o : src/Dao/Stack.hi
src/Dao/Evaluator.o : src/Dao/Runtime.hi
src/Dao.o : src/Dao.hs
src/Dao.o : src/Dao/Object/PPrint.hi
src/Dao.o : src/Dao/PPrint.hi
src/Dao.o : src/Dao/Evaluator.hi
src/Dao.o : src/Dao/Tree.hi
src/Dao.o : src/Dao/Procedural.hi
src/Dao.o : src/Dao/Object.hi
src/Dao.o : src/Dao/Glob.hi
src/Dao.o : src/Dao/String.hi
src/Dao.o : src/Dao/Runtime.hi
tests/main.o : tests/main.hs
tests/main.o : tests/Dao/Test.hi
src/dao-main.o : src/dao-main.hs
src/dao-main.o : src/Dao/Evaluator.hi
src/dao-main.o : src/Dao.hi
# DO NOT DELETE: End of Haskell dependencies
