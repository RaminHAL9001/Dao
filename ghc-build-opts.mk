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

shell:=bash
slash:=/
dot:=.
star:=*
hash:=\#

listfile = grep -v '^[[:space:]]*$(hash).*$$' $1

.PHONEY:  edit  default  debug  clean

####################################################################################################
# The default target

default: dao

####################################################################################################
# The 'edit' target conveniently opens all the files you want to edit in the vim editor.

export GHCRTS := -M3G #Allow 3GB of heap space to GHC when compiling.

EDIT_FILES_LIST := edit-files.list
EDIT_SCRATCH    := scratch.hs -c ':set autowrite autoread'
NUMBER_OF_TABS  := 6

edit: $(EDIT_FILES_LIST)
	vim -p$(NUMBER_OF_TABS) \
		$(EDIT_FILES_LIST) \
		$(shell $(call listfile,$(EDIT_FILES_LIST)))

$(EDIT_FILES_LIST):
	@echo 'Create edit-files.list'
	@(	echo '# A list of files you want to edit with Vim.'
		echo '# They will appear in the order specified.'
		echo 'ghc-build-opts.mk';
		find ./ -name '*.hs' | sed -e 's,^[.]/,,'; \
	) >edit-files.list

####################################################################################################
# Building the actual Dao intepreter program.

DAO_PROJECT_FILES_LIST := project-files.list
DAO_PROJECT_FILES      := $(shell $(call listfile,$(DAO_PROJECT_FILES_LIST)))
DAO_INCLUDES   = -i'./src' -i'./tests'

GHC_BUILD_OPTS   = -threaded
GHC_COMPILE      = ghc $(DAO_INCLUDES) $(GHC_BUILD_OPTS) --make
GHC_COMPILE_PROF = $(GHC_COMPILE) -rtsopts -prof

ifndef DAO_PROJECT_FILES
$(error $(DAO_PROJECT_FILES) list is empty)
endif

DAO_DEPENDS     := $(patsubst $(star)%,%,$(DAO_PROJECT_FILES))
DAO_MODULES     := $(subst $(slash),$(dot),$(patsubst %.hs,%,$(DAO_DEPENDS)))
DAO_DEPENDS_SRC := $(addprefix src/,$(DAO_DEPENDS))

dao: $(DAO_DEPENDS_SRC)
	@echo 'Building project...'
	$(GHC_COMPILE) $(DAO_DEPENDS_SRC) -o ./dao;

dao-prof: $(DAO_DEPENDS_SRC)
	@echo 'Building project...'
	$(GHC_COMPILE_PROF) $(DAO_DEPENDS_SRC) -o ./dao-prof

clean:
	rm dao; find . \( -name '*.o' -o -name '*.hi' \)  -delete -print

####################################################################################################
# Testing modules

PARSER_TEST_FILES := src/Dao/EnumSet.hs src/Dao/Parser.hs src/Dao/Object/Parser.hs
parser-test: $(PARSER_TEST_FILES)
	$(GHC_COMPILE) $(PARSER_TEST_FILES) -o ./parser-test

ENUM_SET_TEST_FILES := src/Dao/EnumSet.hs tests/I.hs tests/EQN.hs tests/TestEnumSet.hs
enum-set-test:
	$(GHC_COMPILE) $(ENUM_SET_TEST_FILES) -o enum-set-test

####################################################################################################
# Other modules. These are mostly for experimenting with new ideas. If you have a new algorithm to
# try, just start writing a new source file, make a target for it here, and modify the 'default'
# target above to use these targets as prerequisites.

DEBUG_DEPENDS := tests/RandObj.hs tests/main.hs \
  src/Dao/String.hs      src/Dao/Token.hs         src/Dao/Predicate.hs \
  src/Dao/Parser.hs      src/Dao/Object.hs        src/Dao/PPrint.hs \
  src/Dao/Object/PPrint.hs src/Dao/Object/Parser.hs src/Dao/Object/Binary.hs

GHC_COMPILE_DEBUG := $(GHC_COMPILE) -rtsopts -with-rtsopts='-M8G -N4'

./debug/test: $(DEBUG_DEPENDS) ghc-build-opts.mk
	mkdir -p ./debug/
	$(GHC_COMPILE_DEBUG) $(DEBUG_DEPENDS) -o ./debug/test

./debug/test-prof: $(DEBUG_DEPENDS) ghc-build-opts.mk
	$(GHC_COMPILE_DEBUG) -prof $(DEBUG_DEPENDS) -o ./debug/test-prof

