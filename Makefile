# "ghc-build-opts.mk": a GNU-Make script to build the "Dao" modules
# and interactive program.
# 
# Copyright (C) 2008-2014  Ramin Honary.
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
GHC_BUILD = $(GHC_CMD) --make $(SRC_DIRS) $(GHC_FLAGS) $(RTS_OPTS) $(LINKER_FLAGS) $(LANG_EXTS)

CHANGED_FILES := $(shell find $(SOURCE_DIRECTORIES) -name '[A-Z]*.hs' -newer ./Makefile)

####################################################################################################

all: dao test

dao: $(CHANGED_FILES)
	$(GHC_BUILD) -o dao src/dao-main.hs $(CHANGED_FILES) $(LINKER_FLAGS)

test: debug debug/test

debug:
	mkdir -p debug

debug/test: tests/main.hs debug $(CHANGED_FILES)
	$(GHC_BUILD) -o debug/test tests/main.hs $(CHANGED_FILES)

clean:
	find $(SOURCE_DIRECTORIES) \( -name '*.o' -o -name '*.hi' \) -printf 'rm %p;\n' -delete;

listfile = grep -v '^[[:space:]]*$(hash).*$$' $1

edit: $(EDIT_FILES_LIST)
	vim -p$(NUMBER_OF_TABS) \
		$(EDIT_FILES_LIST) \
		$(shell $(call listfile,$(EDIT_FILES_LIST)))

NOTE := $(hash)$(hash)$(hash)

info:
	@echo '$(NOTE) GHC Build command is:'
	@echo '$(NOTE) $(GHC_BUILD)'

