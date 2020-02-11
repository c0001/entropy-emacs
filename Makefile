EMACS_MAKE=emacs --batch -l init.el

help:
	@echo "======================================================================"
	@echo "                         Entropy-Emacs Make"
	@echo ""
	@echo "Welcom to Entropy-Emacs Make procedure, there's four 'make' options"
	@echo "provision, 'all', 'install', 'update' and 'dump', for each as install"
	@echo "all packges and prompting for updaing packags and dumping emacs, or"
	@echo "install all packags or just updating pre-request packages, dump option"
	@echo "for just dumping current eemacs status (notice for using 'install'"
	@echo "operation firstly for check requirements dependencies or using 'all'"
	@echo "operation dismiss the mistake steps instead)."
	@echo ""
	@echo "Example:"
	@echo ""
	@echo "       make all"
	@echo "       make install"
	@echo "       make update"
	@echo "       make dump"
	@echo ""
	@echo ""
	@echo "The dumped emacs binary file is stored in your 'user-emacs-directory'"
	@echo "with name-space as 'eemacs_YearDateTime.pdmp', call it's with:"
	@echo ""
	@echo "-------"
	@echo "        emacs --dump-file=eemacs_YearDateTime.pdmp"
	@echo "-------"
	@echo "======================================================================"
	@echo ""

install:
	@export EEMACS_MAKE=Install;\
	$(EMACS_MAKE)

update:
	@export EEMACS_MAKE=Update;\
	$(EMACS_MAKE)

dump:
	@export EEMACS_MAKE=Dump;\
	$(EMACS_MAKE)

all:
	@export EEMACS_MAKE=Install;\
	$(EMACS_MAKE)

	@export EEMACS_MAKE=Update;\
	$(EMACS_MAKE)

	@export EEMACS_MAKE=Dump;\
	$(EMACS_MAKE)



