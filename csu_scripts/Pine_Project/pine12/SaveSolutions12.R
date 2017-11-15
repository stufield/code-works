################################
###### pine12 Wrapper ##########
###### For Solutions ###########
################################
### Stu Field
###	Department of Biology
###	Colorado State University
###	Fort Collins, CO 80523-1878
###	sgf@colostate.edus
### Stu Field
####################
#rm(list=ls())
####################
source("/Users/sfield/Documents/Dropbox/R-scripts/Pine Project/pine12/pine12.R")
ls()
########################################################
Gen2 <- pine12(Gen=2, plot=FALSE)
Gen10 <- pine12(Gen=10, plot=FALSE)
Gen100 <- pine12(Gen=100, plot=FALSE)
Gen101 <- pine12(Gen=101, plot=FALSE)
Gen1000 <- pine12(Gen=1000, plot=FALSE)
Gen1001 <- pine12(Gen=1001, plot=FALSE)
save(Gen2,Gen10,Gen100,Gen101,Gen1000,Gen1001, file="pine12_solutions.rda")
list.files()
############################################################################


