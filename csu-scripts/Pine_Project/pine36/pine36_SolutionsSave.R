################################
###### pine36 Wrapper ##########
###### For Solutions ###########
################################
### Stu Field
###	Department of Biology
###	Colorado State University
###	Fort Collins, CO 80523-1878
###	sgf@colostate.edus
### Stu Field
### Dec 16th 2010
####################
rm(list=ls())
####################
source("~/Documents/Dropbox//R-scripts/Pine Project/pine36/pine36.R")
ls()
#############################################################################
Gen2 <- pine36(Gen=2)
Gen10 <- pine36(Gen=10)
Gen100 <- pine36(Gen=100)
Gen101 <- pine36(Gen=101)
Gen1000 <- pine36(Gen=1000)
Gen1001 <- pine36(Gen=1001)
save(Gen2,Gen10,Gen100,Gen101,Gen1000,Gen1001, file="pine36_Solutions.rda")
list.files()
############################################################################
#load("pine36_solutions.rda")




