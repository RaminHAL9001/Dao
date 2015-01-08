# "ghc-build-opts.mk": a trivial GNU-Make script that calls 'cabal configure' and 'cabal build'.
#
# Copyright (C) 2008-2015  Ramin Honary.
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

.PHONEY: all ghci test install edit clean

GHC_FLAGS := \
	-threaded                    \
	-Wall                        \
	-fno-warn-name-shadowing     \
	-fno-warn-unused-do-bind     \
	-XDeriveDataTypeable         \
	-XExistentialQuantification  \
	-XFlexibleContexts           \
	-XFlexibleInstances          \
	-XFunctionalDependencies     \
	-XGeneralizedNewtypeDeriving \
	-XMultiParamTypeClasses      \
	-XOverlappingInstances       \
	-XRankNTypes                 \
	-XScopedTypeVariables        \
	-XStandaloneDeriving         \
	-XTemplateHaskell            \
	-XTypeFamilies               \

GHC_SOURCES = -i./src -i./tests -i./

GHC_MODULES = \
	Dao.Array    Dao.Interval   Dao.Eval          Dao.Glob        \
	Dao.IOArray  Dao.Grammar    Dao.Predicate     Dao.Ref         \
	Dao.TestNull Dao.Text       Dao.Tree          Dao.Object      \
	Dao.Lang     Dao.EnumMap    Dao.SparseArray   Dao.Text.Parser \
	Dao.Range    Dao.Count      Dao.Text.Builder  Dao.Int         \
	Dao.Check    Dao.Concurrent Dao.Grammar.Debug Dao.PPrint      \
	Dao.Computer

GHC := ghc --make $(GHC_FLAGS) $(GHC_SOURCES)

all: dist
	cabal build
#all:
#	ghc --make -i./src Dao.Lens -XMultiParamTypeClasses -XFlexibleInstances -XFlexibleContexts

dist: Dao.cabal
	cabal configure
	@echo '----------------------------------------------------------------------------------------------------'

test:
	cabal test

edit:
	vim Dao.cabal $$( find . -type f -name '*.hs' )

ghci:
	ghci $(GHC_FLAGS) $(GHC_SOURCES) $(GHC_MODULES)

clean:
	cabal clean

install:
	cabal install --user

