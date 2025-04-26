
TOUCH = touch
MV = mv -f
RM = rm -rf
CP = cp
RCMD = R --vanilla CMD
RSCRIPT = Rscript --vanilla
LATEX = latex
PDFLATEX = pdflatex --interaction=batchmode
BIBTEX = bibtex
MAKEIDX = makeindex

.PRECIOUS: Makefile

%.pdf: %.tex
	@ cp sqs-template.tex $*.tex
	$(PDFLATEX) $*
	$(PDFLATEX) $*

%.tex: sqs-params.txt *.R
	@ $(RSCRIPT) \
		-e "source('env.R')" \
		-e "sqs_wrapper(skip_cal = TRUE)"

clean:
	@ $(MV) *.log *.aux *.out sqs-params.tex /tmp
