# "ghc-build-opts.mk": a GNU-Make script to configure the parameters in
# Makefile without modifying the Makefile.
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

EDIT_FILES_LIST = edit-files.list
NUMBER_OF_TABS  = 5

SOURCE_DIRECTORIES =         \
	./src                    \
	./tests                  \

LANGUAGE_EXTENSIONS =        \
	TemplateHaskell          \
	ScopedTypeVariables      \
	RankNTypes               \
	MultiParamTypeClasses    \
	FunctionalDependencies   \
	FlexibleInstances        \
	FlexibleContexts         \

BUILTIN_RTS_OPTIONS = -M8G -N4

ALLOW_CHANGE_RTS_OPTIONS = true

GHC_FLAGS = -threaded -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind

LINKER_FLAGS = # -dynamic # -shared

