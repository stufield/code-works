#######################
#  General
#######################
MV = mv -f
RM = rm -rf
CP = cp
TOUCH = touch
RCMD = R --vanilla CMD
RSCRIPT = Rscript --vanilla
INSTALL = install
LATEX = latex
PDFLATEX = pdflatex
BIBTEX = bibtex
MAKEIDX = makeindex

.PRECIOUS: Makefile

all:  

%.pdf: %.tex
	$(PDFLATEX) $*
	$(PDFLATEX) $*

%.tex: sqs-data.txt lib.R
	@ $(RSCRIPT) \
		-e "load('objects.rda')" \
		-e "source('~/.source.packages.R')" \
		-e "source.packages()" \
		-e "source('lib.R')" \
		-e "sqs_wrapper(skip_cal = TRUE)"

objects: lib.R
	@ $(RSCRIPT) \
		-e "load('~/SomaPackages/SomaGlobals/data/col.string.rda')" \
		-e "load('~/SomaPackages/SomaGlobals/data/col.string.list.rda')" \
		-e "load('~/SomaPackages/somaStu/data/col.string2.rda')" \
		-e "load('~/SomaPackages/somaStu/data/par.def.rda')" \
		-e "load('~/SomaPackages/somaStu/data/mySSmenu.rda')" \
		-e "load('~/SomaPackages/SomaData/data/SSmenu.df.rda')" \
		-e "load('~/SomaPackages/SomaNormalization/data/HybRef.rda')" \
		-e "save(list=ls(), file='~/soma_sciences/sqs/objects.rda')"

clean:
	$(MV) *.log *.aux *.out /tmp
