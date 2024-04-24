# h/t to @jimhester and @yihui for this parse block:
# https://github.com/yihui/knitr/blob/dc5ead7bcfc0ebd2789fe99c527c7d91afb3de4a/Makefile#L1-L4
# Note the portability change as suggested in the manual:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages
#PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
#PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
#PKGSRC  := $(shell basename `pwd`)
RM = rm -rf
RSCRIPT = Rscript --vanilla
ROOTDIR = $(R_SOMAVERSE) 

all: networks somaverse

networks:
	@ echo "Creating package dependency networks via: $(ROOTDIR)"
	@ $(RSCRIPT) pkg-networks.R
	@ echo "All '*.html' files updated ..."

# Run as one-off pkg
somaverse:
	@ echo "Creating somaverse networks via: $(ROOTDIR)"
	@ $(RSCRIPT) \
	-e "rootdir <- gsub('/+$$', '', commandArgs(TRUE))" \
	-e "pkgnet::CreatePackageReport(" \
	-e "  pkg_name    = 'somaverse'," \
	-e "  pkg_path    = file.path(rootdir, 'somaverse')," \
	-e "  report_path = 'somaverse.html'" \
	-e ")" $(ROOTDIR)
	@ echo "somaverse.html' updated ..."
