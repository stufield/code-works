#################################################
################ Plotting #######################
#################################################
### a = Effect of parameter on final (Gen) tree population by class & infection status
### b = Effect of parameter on final (Gen) % infected trees
### c = Effect of parameter on popn trajectory by class
### d = Effect of parameter on prevalence by class 
#################################################
### Load sources for two main functions required for ParExplore ##############
source("/Users/sfield/Documents/CSU/R-scripts/5NP R Code/Animal/runpine6.R") # runpine6 function
source("/Users/sfield/Documents/CSU/R-scripts/5NP R Code/Animal/plotgraphs.R") # plotgraphs function
#################################################
Parameter <- c(0, 0.01, 0.02, 0.1, 0.2)
### Parameter can be any length, but plots are set up to handle length = 5
ParName = "Beta"         # Set the name of the parameter to explore for graphs
Loop <- length(Parameter)
Gen = 100
Plot.Mat = runpine6(Gen, Parameter) # (Gen, alpha)  Create the plotting matrix

#####################
### Call plots ######
### fn(a,b,c,d) #####
#####################
plotgraphs(1,1,1,0)

#########################
### Save Plotting Matrix
#########################
# write.csv(Plot.Mat, "PlottingMatrix.csv", row.names= F)
