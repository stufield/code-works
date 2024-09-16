# h/t to @jimhester and @yihui for this parse block:
# https://github.com/yihui/knitr/blob/dc5ead7bcfc0ebd2789fe99c527c7d91afb3de4a/Makefile#L1-L4
# Note the portability change as suggested in the manual:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages
# PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
# PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
# PKGSRC  := $(shell basename `pwd`)
MV = mv -f
RM = rm -rf
CP = cp
TOUCH = touch
REXE = R --vanilla
RCMD = $(REXE) CMD
RSCRIPT = Rscript --vanilla
INSTALL = install
LATEX = latex
PDFLATEX = pdflatex
BIBTEX = bibtex
#########################
.PRECIOUS: %.tex
.PRECIOUS: Makefile
#########################

all: symbols run clean

symbols: run
	$(PDFLATEX) symbols
	$(PDFLATEX) symbols

run:
	@ $(RSCRIPT) \
	-e "source('lib.R', keep.source = TRUE)" \
	-e "plotSymbols('fig1-symb.pdf')" \
	-e "plotPCHsymbols('fig2-pch-symb.pdf')" \
	-e "print2file(plotSymbols, file = 'plotSymbols.R')" \
	-e "print2file(plotPCHsymbols, file = 'plotPCHsymbols.R')"
	@ sed -i 's/[{]/\\{/g' plotSymbols.R
	@ sed -i 's/[}]/\\}/g' plotSymbols.R
	@ sed -i '/bytecode/d' plotSymbols.R
	@ sed -i '1i\\' plotSymbols.R  # add empty line at top
	@ sed -i 's/[{]/\\{/g' plotPCHsymbols.R
	@ sed -i 's/[}]/\\}/g' plotPCHsymbols.R
	@ sed -i '/bytecode/d' plotPCHsymbols.R
	@ sed -i '1i\\' plotPCHsymbols.R  # add empty line at top

%.pdf: %.tex
	$(PDFLATEX) $*
	$(PDFLATEX) $*

clean:
	@ $(RM) *.log *.aux *.toc *.blg *.bbl *.out

