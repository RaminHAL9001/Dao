# "dao-ghc-build-opts.mk": a GNU-Make script to build the "Dao" modules
# and interactive program.
# 
# Copyright (C) 2008-2012  Ramin Honary.
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

DAO_BUILD_OPTS = -threaded
DAO_COMPILE = ghc $(DAO_BUILD_OPTS) --make

DAO_PROJECT_FILES_LIST = dao-project-files.list

LOAD_PROJECT_FILES = \
		grep -v '^[[:space:]]*\#' $(DAO_PROJECT_FILES_LIST) \
	|	sed -e 's,[[:space:]]*\#.*$$,,' -e 's,[[:space:]]\+,\n,g' \
	|	sort -u

DAO_PROJECT_FILES := $(shell $(LOAD_PROJECT_FILES))

EDIT_FILES_LIST := dao-edit-files.list

LOAD_EDIT_FILES = \
	if test -r '$(EDIT_FILES_LIST)'; then cat '$(EDIT_FILES_LIST)'; fi

DAO_EDIT_FILES = $(shell $(LOAD_EDIT_FILES))

ifndef DAO_PROJECT_FILES
$(error $(DAO_PROJECT_FILES) list is empty)
endif

DAO_DEPENDS := $(patsubst $(star)%,%,$(DAO_PROJECT_FILES))
DAO_MODULES := $(subst $(slash),$(dot),$(patsubst %.hs,%,$(DAO_DEPENDS)))
DAO_GHCI_ADD_FILES := $(filter $(star)%,$(DAO_PROJECT_FILES))
DAO_GHCI_MODULES := $(subst $(slash),$(dot),$(patsubst %.hs,%,$(patsubst $(star)%,%,$(DAO_GHCI_ADD_FILES))))

DAO_DEPENDS_SRC := $(addprefix src/,$(DAO_DEPENDS))

.PHONEY:  edit  all

####################################################################################################

all:  dao

dao: $(DAO_DEPENDS_SRC)
	@echo 'Building project...'
	$(DAO_COMPILE) $(DAO_DEPENDS_SRC) -o ./dao;

calc: src/Dao/Calc.hs src/Dao/Object/Parsers.hs src/Dao/Combination.hs src/Dao/Combination/Parser.hs
	$(DAO_COMPILE) src/Dao/Calc.hs -o ./calc

edit:
	vim -p4 scratch.hs -c ':set autowrite autoread' \
		dao-ghc-build-opts.mk dao-project-files.list \
		$(DAO_DEPENDS_SRC) \
		$(DAO_EDIT_FILES)

