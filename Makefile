include defns.mk
CABAL_FILE ?= cabal

####################################################################################################

PROJECT_NAME    := $(shell grep ^name: -i $(CABAL_FILE) | sed -e 's,^name:[[:space:]]*,,i')
PROJECT_VERSION := $(shell grep ^version: -i $(CABAL_FILE) | sed -e 's,version:[[:space:]]*,,i')
PROJECT_DIR     := $(PROJECT_NAME)
RELEASE_NAME    := $(PROJECT_NAME)-$(PROJECT_VERSION)
ARCHIVE_NAME    := $(RELEASE_NAME).tar.gz

QSOURCES := $(shell find src -type f -name '*.hs' -printf \''%p'\''\n'; )
SOURCES  := $(shell for src in $(QSOURCES); do printf '%s\n' "$${src}"; done; )

LIBRARY := dist/build/$(PROJECT_NAME)
export VIM_SESSION := .$(PROJECT_NAME).vim

.PHONEY: all install clean edit configure config archive

# This exists because it is easier to use than figure out how to make vim
# properly parse compile time error log messages from GHC.
ifdef MODULE_TOP
FILTER := | \
    sed -e 's,^$(MODULE_TOP)/[^.]\+[.]hs:[0-9:]\+.*$$,& ,' \
        -e 's,[(]bound at \($(MODULE_TOP)/[^.]\+[.]hs:[0-9:]\+\)[)],...bound at...\n\1: \n,' \
        -e 's,^[[:space:]]*at \($(MODULE_TOP)/[^.]\+[.]hs:[0-9:]\+\),...at...\n\1: ,' \
		-e 's,^cabal:[[:space:]]*\($(PROJECT_NAME).cabal:[0-9]\+:.*\),\1 ,' \
		-e 's/^[[:space:]]*error, called at /error, called at\n/'
endif

ifdef TESTING
CONFIG_OPTS := --enable-tests --enable-coverage
CABAL_BUILD := cabal test
else
CABAL_BUILD := cabal build
endif

$(LIBRARY): configure $(SOURCES)
	$(CABAL_BUILD) 2>&1 $(FILTER)

install: $(LIBRARY)
	cabal install --force-reinstalls | $(FILTER)

clean:
	cabal clean

config: dist

configure: dist

dist: $(CABAL_FILE)
	cabal configure $(CONFIG_OPTS)

edit:
	if [ -f '$(VIM_SESSION)' ]; \
	then vim -S '$(VIM_SESSION)'; \
	else vim '$(CABAL_FILE)' $(QSOURCES) 'Makefile'; \
	fi;

TAGS tags: $(SOURCES)
	hasktags $(QSOURCES)

../$(ARCHIVE_NAME): $(LIBRARY)
	mkdir -p ../$(RELEASE_NAME); \
	if tar cf - $(CABAL_FILE) Setup.hs LICENSE $(QSOURCES) | \
			tar xvf - -C ../'$(RELEASE_NAME)' | sed -e 's,^,COPY: ,'; \
	then \
		if tar czvf ../'$(ARCHIVE_NAME)' --format=ustar -C ../ '$(RELEASE_NAME)' | sed -e 's,^,ARCHIVE: ,'; \
		then rm -Rf ../'$(RELEASE_NAME)'; \
		fi; \
	fi;

archive: ../$(ARCHIVE_NAME)

