# * Commentary
# ==============================================================
#                    Entropy-Emacs Make
#
# Welcom to Entropy-Emacs Make procedure, there's four 'make'
# options provision that 'install', 'install-coworkers','update'
# and 'dump', for each as install all elisp packges, install
# external depedencies and prompting for updaing packags and
# dumping emacs, or doing thus all using 'all' option, dump option
# for just dumping current eemacs status (notice for using
# 'install' operation firstly for check requirements dependencies
# or using 'all' operation dismiss the mistake steps instead).
#
# Example:
#
#        make all
#        make install
#        make install-coworkers
#        make update
#        make dump
#
#
# The dumped emacs binary file is stored in your 'user-emacs-directory'
# with name-space as 'eemacs_YearDateTime.pdmp', call it's with:
#
# -------
#        emacs --dump-file=eemacs_YearDateTime.pdmp
# -------
# ==============================================================
#
# * Code
EMACS := emacs
EMACS_MAKE=$(EMACS) --batch -l init.el

help:
	$(info Please cat commentry of this makefile for usage prompt)

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
