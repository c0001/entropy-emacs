EMACS_MAKE=emacs --batch -l init.el

help:

	@echo "======================================================================"
	@echo "                         Entropy-Emacs Make"
	@echo ""
	@echo "Welcom to Entropy-Emacs Make procedure, there's three 'make' options"
	@echo "provision, 'all', 'install' and 'update', for each as install all"
	@echo "packges and prompting for updaing packags and dumping emacs, or"
	@echo "install all packags or just updating pre-request packages."
	@echo ""
	@echo "Example:"
	@echo ""
	@echo "       make all"
	@echo "       make install"
	@echo "       make update"
	@echo ""
	@echo ""
	@echo "The dumped emacs binary file is stored in your 'user-emacs-directory'"
	@echo "with name-space as 'eemacs_YearDateTime.pdmp', call it's with:"
	@echo ""
	@echo '```'
	@echo "        emacs --dump-file=eemacs_YearDateTime.pdmp"
	@echo '```'
	@echo "======================================================================"



all:
	@export EEMACS_MAKE=All;\
	$(EMACS_MAKE)

install:
	@export EEMACS_MAKE=Install;\
	@$(EMACS_MAKE)

update:
	@export EEMACS_MAKE=Update;\
	@$(EMACS_MAKE)

