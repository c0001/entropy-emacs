# Compile elisp into native code.
#    Copyright (C) 2018-2020 entropy-emacs
#
# Author: Entropy <bmsac0001@gmail.com>
#
# This file is part of Entropy-Emacs.
#
# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
#
# * code
# ** os detecting
# OS detected method obtained by https://stackoverflow.com/questions/714100/os-detecting-makefile
ifeq '$(findstring ;,$(PATH))' ';'
    detected_OS := Windows
else
    detected_OS := $(shell uname 2>/dev/null || echo Unknown)
    detected_OS := $(patsubst CYGWIN%,Cygwin,$(detected_OS))
    detected_OS := $(patsubst MSYS%,MSYS,$(detected_OS))
    detected_OS := $(patsubst MINGW%,MSYS,$(detected_OS))
endif

# ** variable
EEMACS_TOP_DIR = $(CURDIR)
EEMACS_SESSION := main
EEMACS_SERVICE_HOST := $(XDG_CONFIG_HOME)/systemd/user
EEMACS_SERVICE_FILE = $(EEMACS_SERVICE_HOST)/eemacs-daemon.$(EEMACS_SESSION).service
EMACS := emacs
EMACS_MAJOR_VERSION := 28
EMACS_RUN = $(EMACS) -Q -l init.el
EMACS_MAKE = $(EMACS) -Q --batch -l init.el
TERMINATE_WARN = $(EMACS) --batch -q --eval '(or (yes-or-no-p "Remember terminate by kill -9 this make process instead of Ctrl-c since curl do not capture SIGINT") (error "force abort!"))'

DEBUG_FORM = '(progn\
  (setq entropy/emacs-startup-benchmark-init t)\
  (setq entropy/emacs-startup-debug-on-error t))'

ifeq ($(detected_OS),Windows)
CAT=type
EchoEmpty=echo.
else
CAT=cat
EchoEmpty=echo
endif

# ** main

.PHONY:help install compile compile-clean install-coworkers install-eemacs-ext-build install-eemacs-fonts update dump native-comp liberime all debug

help: export EEMACS_MAKE=Help
help:
	@$(EchoEmpty)
	@$(EMACS_MAKE)

install: export EEMACS_MAKE=Install
install:
	@$(EchoEmpty)
	@$(EMACS_MAKE)

compile: export EEMACS_MAKE=Compile
compile:
	@$(EchoEmpty)
	@$(EMACS_MAKE)

compile-clean: export EEMACS_MAKE=Compile-Clean
compile-clean:
	@$(EchoEmpty)
	@$(EMACS_MAKE)

install-coworkers: export EEMACS_MAKE=Install-Coworkers
install-coworkers:
	@$(EchoEmpty)
	@$(EMACS_MAKE)

install-eemacs-ext-build: export EEMACS_MAKE=Install-Eemacs-Ext-Build
install-eemacs-ext-build:
	@$(EchoEmpty)
	@$(EMACS_MAKE)

install-eemacs-fonts: export EEMACS_MAKE=Install-Eemacs-Fonts
install-eemacs-fonts:
	@$(EchoEmpty)
	@$(TERMINATE_WARN)
	@$(EMACS_MAKE)

update: export EEMACS_MAKE=Update
update:
	@$(EchoEmpty)
	@$(EMACS_MAKE)

dump: export EEMACS_MAKE=Dump
dump:
	@$(EchoEmpty)
	@$(EMACS_MAKE)

native-comp: export EEMACS_MAKE=Native-Comp
native-comp:
	@$(EchoEmpty)
	@$(EMACS_MAKE)

liberime: export EEMACS_MAKE=Liberime
liberime:
	@$(EchoEmpty)
# liberime
	make EMACS='' EMACS_MAJOR_VERSION=$(EMACS_MAJOR_VERSION) -C elements/site-lisp/liberime/ clean
	make EMACS='' EMACS_MAJOR_VERSION=$(EMACS_MAJOR_VERSION) -C elements/site-lisp/liberime/ all
# emacs-rime
	make EMACS='' EMACS_MAJOR_VERSION=$(EMACS_MAJOR_VERSION) -C elements/site-lisp/emacs-rime/ clean
	make EMACS='' EMACS_MAJOR_VERSION=$(EMACS_MAJOR_VERSION) -C elements/site-lisp/emacs-rime/ lib

all: export EEMACS_MAKE_ALL=1
all: install install-coworkers liberime compile-clean compile

install-systemd-service:
	awk "{gsub(\"<<<<INIT>>>>\",  \"$(EEMACS_TOP_DIR)/init.el\")}1"   \
		annex/eemacs-daemon/eemacs-daemon.service.template      | \
	 awk "{gsub(\"<<<<EMACS>>>>\",  \"$(EMACS)\")}1"                | \
	 awk "{gsub(\"<<<<EEMACS_DIR>>>>\",  \"$(EEMACS_TOP_DIR)\")}1"  | \
	 awk "{gsub(\"<<<<SESSION>>>>\",  \"$(EEMACS_SESSION)\")}1"       \
		> annex/eemacs-daemon/eemacs-daemon.service
	install -d "$(EEMACS_SERVICE_HOST)"
	install annex/eemacs-daemon/eemacs-daemon.service "$(EEMACS_SERVICE_FILE)"
	systemctl --user daemon-reload
	@echo "Install to $(EEMACS_SERVICE_FILE) done"

debug: export EEMACS_DEBUG=1
debug:
	@$(EchoEmpty)
	@$(EMACS_RUN) --debug-init
