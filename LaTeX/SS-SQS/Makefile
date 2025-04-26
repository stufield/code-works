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

%.tex: sqs-params.txt env.R
	@ $(RSCRIPT) \
		-e "source('env.R')" \
		-e "sqs_wrapper(skip_cal = TRUE)"

clean:
	$(MV) *.log *.aux *.out /tmp
