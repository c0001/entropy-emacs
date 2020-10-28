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
EMACS := emacs
EMACS_MAKE=$(EMACS) --batch -l init.el

ifeq ($(detected_OS),Windows)
CAT=type
else
CAT=cat
endif

# ** main
help:
	@$(CAT) ./make-help.txt

install:
	$(info )
	@export EEMACS_MAKE=Install;\
	$(EMACS_MAKE)


install-coworkers:
	$(info )
	@export EEMACS_MAKE=Install-Coworkers;\
	$(EMACS_MAKE)

update:
	$(info )
	@export EEMACS_MAKE=Update;\
	$(EMACS_MAKE)

dump:
	$(info )
	@export EEMACS_MAKE=Dump;\
	$(EMACS_MAKE)

native-comp:
	$(info )
	@export EEMACS_MAKE=native-comp;\
	$(EMACS_MAKE)

all:	install	install-coworkers update dump
