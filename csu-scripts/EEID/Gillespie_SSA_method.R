# This R script reproduces the examples from:
#   Mario Pineda-Krch
#   GillespieSSA: Implementing the Gillespie Stochastic Simulation Algorithm in R
#   Journal of Statistical Software, 25(12), April 2008

# Note that these are large scale simulations running up to 10,000 iteration per
# SSA method and per model --- in other words, it will take a *very long* time
# to run this script. On the computer cluster used to run the examples in the
# manuscript (see paper for further details) the simulations took
# approximatelly one week worth of computational time. Note that all the example
# models presented in the JSS paper are also provided as demo models in the
# GillespieSSA package, most having the same parameters. The demo models are
# much faster to run since they only run single realizations for relativelly
# short durations.

# Data from the simulations is saved in the result folder (which is created on
# the fly if it does not exist)

# Clean up to avoid clashes
rm(list=ls())

# Option for running short test runs for diagnostic purposes
test <- FALSE

##########################################
# Define the different biological models #
##########################################

##
# lgr (Logistic growth model)
##
lgr <- function(method="D", tf=100, verbose,
                consoleInterval, epsilon = 0.25) {

  parms <- c(b=2, d=1, K=1000)       # Parameters
  x0  <- c(N=500)                    # Initial state vector
  nu  <- matrix(c(+1,-1),ncol=2)     # State-change matrix
  a   <- c("b*N", "(d+(b-d)*N/K)*N") # Propensity vector

  if (test) tf <- 10

  # Run one realization
  out <- ssa(x0, a, nu, parms, tf, method, simName="Logistic growth",
             tau=0.05,        # ETL
             f=1000,          # BTL
             epsilon=epsilon, # OTL
             nc=10,           # OTL
             hor=c(1),        # OTL
             dtf=10,          # OTL
             nd=100,          # OTL
             verbose=verbose,
             consoleInterval=consoleInterval)
 return(out)
} # lgr()




##
# rma (Predator-prey model)
##
rma <- function(method="D", tf=1000, verbose, consoleInterval, censusInterval=0) {

  # Define parameters
  # (B in Figure 1 in Pineda-Krch et al. 2007)
  parms <- c(b=2, d=1, K=1000, alpha=0.005, w=0.0031, c=1, g=0.8064516)

  # Define system
  x0  <- c(N=500, P=250)               # Initial state vector
  nu  <- matrix(c(+1, -1, -1,  0,  0,   # State-change matrix
                   0,  0,  0, +1, -1),
                   nrow=2,byrow=TRUE)
  # Propensity vector
  a   <- c("b*N",                 # Prey birth
           "(d+(b-d)*N/K)*N",     # Prey death
           "alpha/(1+w*N)*N*P",   # Prey death due to predation
           "c*alpha/(1+w*N)*N*P", # Pred birth
           "g*P")                 # Pred death

  # Run one realization
  out <- ssa(x0, a, nu, parms, tf, method, simName="Rosenzweig-MacArthur predator-prey model",
             tau=0.05,        # ETL
             f=100,           # BTL
             nc=10,           # OTL
             hor=c(2,2),      # OTL
             dtf=10,          # OTL
             nd=100,          # OTL
             verbose=verbose,
             consoleInterval=consoleInterval,
             censusInterval=censusInterval,
             maxWallTime=36000)

  return(out)
}



##
# epiChain (SIRS metapopulation model)
##
epiChain <- function(method = "D", tf = 1000, verbose = TRUE,
                     consoleInterval = 0, censusInterval = 0,
                     epsilon = 0.25, tau = 0.3) {

  # Define parameters
  parms <- c(beta = 0.001, # Transmission rate
            gamma = 0.1,   # Recovery rate
              rho = 0.005, # Loss of immunity rate
          epsilon = 0.01,  # Proportion inter-patch transmissions
                N = 500)   # Patch population size (constant)

  # Initial state vector
  U <- 100 # Number of patches
  x0 <- c(S1=(parms[["N"]]-1), I1=1)
  if (U>1) {
    for (i in (seq(2,U))) {
      patch <- c(parms[["N"]], 0)
      names(patch) <- c(paste("S",i,sep=""), paste("I",i,sep=""))
      x0 <- c(x0, patch)
    }
  }

  # State-change matrix
  # Define the state change matix for a single patch
  #      Reaction 1   2   3   4     State
  nu <- matrix(c(-1, -1,  0, +1,  # S
                 +1, +1, -1,  0), # I
               nrow=2, byrow=TRUE)

  ###
  # Uncomment the next section to create the full 'nu' matrix (no nu tiling)
  ###
  # Tile the single patch state change matrix into the system wide state change matrix
  #nu <- NULL
  #j <- 1
  #for (patch in (seq(U))) {
  #  nu_row <- matrix(rep(0,U*4*2),nrow=2)
  #  nu_row[,j:(j+3)] <- patch_nu
  #  nu <- rbind(nu, nu_row)
  #  j <- j + 4
  #}
  ###

  # Create the propensity vector
  a <- NULL
  for (patch in (seq(U))) {
    i <- patch            # Intra-patch index
    if (patch==1) j <- U  # Inter-patch index
    else j <- patch-1

    # Construct the propensity functions for the current patch       # Reaction:
    a_patch <- c(paste("(1-epsilon)*beta*S",i,"*I",i,sep=""), # 1. Intra-patch infection
                 paste("epsilon*beta*S",i,"*I",j,sep=""),     # 2. Inter-patch infection
                 paste("gamma*I",i,sep=""),                     # 3. Recovery from infection
                 paste("rho*(N-S",i,"-I",i,")",sep=""))          # 4. Loss of immunity
    a <- c(a, a_patch)
  } # for()

  # Run one realization
  out <- ssa(x0,a,nu,parms,tf,method,simName="Daisy chain SIRS model",
             tau=tau,                # ETL
             f=10,                   # BTL
             epsilon=epsilon,        # OTL
             nc=10,                  # OTL
             hor=rep(2,length(x0)),  # OTL
             dtf=10,                 # OTL
             nd=100,                 # OTL
             verbose=verbose,
             censusInterval=censusInterval,
             consoleInterval=consoleInterval)

  return(out)
}

################################################################################
# Auxilliary functions                                                         #
################################################################################
createResultFolder <- function(model,method) {

  # Try to create the top-level result folder
  createdSucessfully = dir.create("results")
  if (!createdSucessfully) warning("could not create 'results' folder - I assume it already exist. Proceeding...")

  # Figure out the folder name where the results will go. To avoid name clashes we number folders with the same base name
  folderExists <- TRUE # Just to get into the while loop
  counter <- 1
  while (folderExists) {
   folderName <- paste("results/",model,".",method,".",counter,sep="")
   folderExists <- file.exists(folderName)
   counter <- counter + 1
  }

  # Create the sub-folder where the results will go.
  createdSucessfully = dir.create(folderName)
  if (!createdSucessfully) stop("could not create ",folderName)
  return(folderName)
}

binification <- function(data,binVector,popSizeCountsBinSize){
  for(i in seq(length(data))){

    # Which bin should the i'th population size go in?
    rawBinNr <- data[i]/popSizeCountsBinSize
    if (rawBinNr>0) binNr <- ceiling(rawBinNr)
    else binNr <- 1

    # Add bins if necessary
    nBins <- length(binVector)
    if (binNr > nBins) binVector <- c(binVector, rep(0,times=(binNr-nBins)))

    # Update the binVector
    binVector[binNr] <- binVector[binNr] + 1
  } # for()
  return(binVector)
} # binification()

################################################################################
# Wrapper function that runs a batch of runs for a given model and method and  #
# collates the relevant simulation results                                     #
################################################################################
main <- function( model = stop("missing model name"),
                 method = stop("missing SSA method"),
          nRealizations = stop("missing number of realizations to run"),
                verbose = TRUE,
        consoleInterval = Inf,
                epsilon = 0.25) # Only OTL
                {
  options(digits=10)
  require("GillespieSSA")

  # Define the path where the results will saved and define misc. census objects.
  # For the OTL method we also append the epsilon value used
  tmp_method <- method
  if ((method=="OTL") | (method=="otl")) method <- paste(method,".e",epsilon,sep="")
  path <- createResultFolder(model,method)
  method <- tmp_method

  # Define miscellaneous data census vectors
  nrOfSteps       <- NULL
  elapsedWallTime <- NULL
  meanPopSize     <- NULL
  sdPopSize       <- NULL
  meanStepSize    <- NULL
  sdStepSize      <- NULL

  # Set the number of populations (states) each model consists of
  switch(model,
         "lgr" = { nrOfPopulations <- 1 },
         "rma" = { nrOfPopulations <- 2 },
         "epiChain" = { nrOfPopulations <- 2000 },
         stop("unknown model")
         )

  # Define the appropriate sized 'popSizeCounts' matrix
  popSizeCounts <- matrix((rep(0,10*nrOfPopulations)),nrow=nrOfPopulations)
  popSizeCountsBinSize <- 20 # Size of bins in 'popSizeCount'

  # Define the appropriate sized 'stepSizeCounts' vector
  stepSizeCounts <- rep(0,10)
  stepSizeCountsBinSize <- 0.01 # Size of bins in 'stepSizeCount'

  # Define the object that will keep track of termination statuses for the runs
  terminationStatuses <- c(0,0,0,0,0)
  names(terminationStatuses) <- c("finalTime","extinction","negativeState","zeroProp","maxWallTime")

  # Keep track of the number of suspended tau-leaps for the OTl method
  if ((method=="OTL") | (method=="otl")) nSuspendedTauLeaps <- NULL

  # Splash screen
  cat("========================================================================\n")
  cat("Starting ",nRealizations," realizations of ",model," model using ",method,"\n",sep="")
  cat("Results will be saved in ",path,"\n",sep="")
  cat("========================================================================\n")
  startWallTime <- format(Sys.time())
  procTimeStart <- as.numeric(proc.time()[3])

  #############################################################################
  # Start runs
  for (realization in seq(nRealizations)) {
    cat("========================================================================\n")
    cat("Realization ",realization," out of ",nRealizations,"...\n",sep="")
    flush.console()

    if (test) tf=5 else tf=100

    switch(model,
           "lgr" = { out <- lgr(method,tf=100,verbose,consoleInterval) },
           "rma" = { out <- rma(method,tf=100,verbose,consoleInterval) },
           "epiChain" = { out <- epiChain(method,tf=100,verbose,consoleInterval) },
           stop("unknown model")
          ) # switch()

    # Save info from the run
    nrOfSteps       <- c(nrOfSteps, out$stats$nSteps)
    elapsedWallTime <- c(elapsedWallTime, out$stats$elapsedWallTime)
    meanStepSize    <- c(meanStepSize, out$stats$meanStepSize)
    sdStepSize      <- c(sdStepSize, out$stats$sdStepSize)

    # Loop over all the pop and save some pop specific information
    meanPopSize_i <- NULL
    sdPopSize_i   <- NULL
    for (i in seq(nrOfPopulations)) {

      # Calculate the mean+/-1sd of the current population
      meanPopSize_i <- c(meanPopSize_i, mean(out$data[,1+i]))
      sdPopSize_i   <- c(sdPopSize_i, sd(out$data[,1+i]))

      # Binify the current population
      ithPop <- binification(out$data[,(1+i)],popSizeCounts[i,],popSizeCountsBinSize)

      # If the 'ithPop' has more columns that the 'popSizeCounts' matrix we need to enlarge it...
      if (length(ithPop)>dim(popSizeCounts)[2]) {
         nMissingCols  <- length(ithPop)-dim(popSizeCounts)[2]
         missingCols   <- matrix(rep(0,(nMissingCols*nrOfPopulations)),nrow=nrOfPopulations)
         popSizeCounts <- cbind(popSizeCounts, missingCols)
      }

      # Update the i'th population
      popSizeCounts[i,] <- ithPop
    } # for()

    meanPopSize <- rbind(meanPopSize, meanPopSize_i)
    sdPopSize <- rbind(sdPopSize, sdPopSize_i)

    # Update the 'stepSizeCounts' vector by binifying the step sizes
    stepSize <- round(diff(out$data[,1]),4)
    stepSize <- stepSize[1:(length(stepSize)-1)] # Need to remove the last step size due to a know bug in GillespieSSA 0.1-0 (see list of known issues)
    stepSizeCounts <- binification(stepSize,stepSizeCounts,stepSizeCountsBinSize)

    # Update the terminationStatuses matrix with the current runs termination status
    terminationStatuses[out$stats$terminationStatus] <- terminationStatuses[out$stats$terminationStatus] + 1

    # For OTL - record the proportion of suspended tau-leaps
    if ((method=="OTL") | (method=="otl")) nSuspendedTauLeaps <- c(nSuspendedTauLeaps, out$stats$nSuspendedTauLeaps/out$stats$nSteps)

  } # for()
  #############################################################################
  # End runs

  procTimeEnd <- as.numeric(proc.time()[3])
  totoElapsedWallTime <- procTimeEnd-procTimeStart
  cat("========================================================================\n")
  cat("Finished ",nRealizations," realizations of ",model," model using ",method,"\n",sep="")
  cat("Results were saved in ",path,"\n",sep="")
  cat("========================================================================\n")

  # Add column names to the popSizeCounts matrix
  names(popSizeCounts) <- as.character(seq(length(popSizeCounts))*popSizeCountsBinSize)

  # Make some plots (that are saved to disk)
  pdf(paste(path,"/nrOfSteps.pdf",sep=""))
    hist(nrOfSteps,25,xlab="Nr of time steps",ylab="Frequency of realizations",main=paste(path,sep=""))
    mtext(paste(nRealizations," realizations, toto elapsed wall time: ",round(totoElapsedWallTime),
                "sec, toto nr of steps: ",sum(nrOfSteps),", duration/step: ",
                round(totoElapsedWallTime/sum(nrOfSteps),4)*1000,"ms, mean nr of steps: ",
                round(mean(nrOfSteps),4),"+/-",round(sd(nrOfSteps),4)," (1sd)",sep=""),cex=0.5)
  dev.off()

  pdf(paste(path,"/elapsedWallTime.pdf",sep=""))
    hist(elapsedWallTime,25,xlab="Elapsed wall time (seconds)",ylab="Frequency of realizations",main=paste(path,sep=""))
    mtext(paste(nRealizations," realizations, toto elapsed wall time: ",round(totoElapsedWallTime),
                "sec, mean elapsed wall time: ",round(mean(elapsedWallTime),4),"sec+/-",
                round(sd(elapsedWallTime),4)," (1sd)",sep=""),cex=0.5)
  dev.off()

  # Generate species specific plots
  for (i in seq(nrOfPopulations)) {

    pdf(paste(path,"/meanPopSize.",names(out$args$x0[i]),".pdf",sep=""))
      hist(meanPopSize[,i],25,xlab=paste(names(out$args$x0[i])," mean population size",sep=""),ylab="Frequency of realizations",main=paste(path,sep=""))
      mtext(paste(nRealizations," realizations",sep=""))
    dev.off()

    pdf(paste(path,"/sdPopSize.",names(out$args$x0[i]),".pdf",sep=""))
      hist(sdPopSize[,i],25,xlab=paste(names(out$args$x0[i]),"+/- 1sd population size",sep=""),ylab="Frequency of realizations",main=paste(path,sep=""))
      mtext(paste(nRealizations," realizations",sep=""))
    dev.off()

    pdf(paste(path,"/popSizeCounts.",names(out$args$x0[i]),".pdf",sep=""))
      barplot(popSizeCounts[i,]/sum(popSizeCounts[i,]),space=0,
              names.arg=as.character(seq(length(popSizeCounts[i,]))*popSizeCountsBinSize),
              xlab=paste(names(out$args$x0[i])," population size",sep=""),
              ylab="Relative frequency",
              main=paste(path,sep=""))
      mtext(paste(nRealizations," realizations",sep=""))
    dev.off()
  } # for()

  pdf(paste(path,"/stepSizeCounts.pdf",sep=""))
    barplot(stepSizeCounts/sum(stepSizeCounts),space=0,
            names.arg=as.character(seq(length(stepSizeCounts))*stepSizeCountsBinSize),
            xlab="Step size",
            ylab="Relative frequency",
            main=paste(path,sep=""))
    mtext(paste(nRealizations," realizations, mean step size: ",round(mean(meanStepSize),4),sep=""))
  dev.off()

  pdf(paste(path,"/terminationStatuses.pdf",sep=""))
    barplot(terminationStatuses/sum(terminationStatuses),
            ylab="Relative frequency",
            xlab="Termination status",
            main=paste(path,sep=""),cex.names=0.75)
    mtext(paste(nRealizations," realizations",sep=""))
  dev.off()

  if ((method=="OTL") | (method=="otl")) {
    pdf(paste(path,"/nSuspendedTauLeaps.pdf",sep=""))
      hist(nSuspendedTauLeaps,25,xlab="Nr of suspended tau-leaps",ylab="Relative frequency",main=paste(path,sep=""))
      mtext(paste(nRealizations," realizations, mean nr of suspended tau-leaps: ",
                  round(mean(nSuspendedTauLeaps),4),"+/-",round(sd(nSuspendedTauLeaps),4)," (1sd)",sep=""))
    dev.off()
  } # if()

  # Save data objects
  write(nrOfSteps,file=paste(path,"/nrOfSteps.txt",sep=""),ncolumns=1,sep="\t")
  write(elapsedWallTime,file=paste(path,"/elapsedWallTime.txt",sep=""),ncolumns=1,sep="\t")
  write(meanPopSize,file=paste(path,"/meanPopSize.txt",sep=""),ncolumns=nrOfPopulations,sep="\t")
  write(sdPopSize,file=paste(path,"/sdPopSize.txt",sep=""),ncolumns=nrOfPopulations,sep="\t")

  fileHead <- (seq(length(popSizeCounts))*popSizeCountsBinSize)
  popSizeCounts <- rbind(fileHead,popSizeCounts)
  write(t(popSizeCounts),file=paste(path,"/popSizeCounts.txt",sep=""),ncolumns=dim(popSizeCounts)[2],sep="\t")

  write(t(rbind(names(terminationStatuses),terminationStatuses)),file= paste(path,"/terminationStatuses.txt",sep=""), ncolumns=length(terminationStatuses),sep="\t")
  if ((method=="OTL") | (method=="otl")) {
    write(nSuspendedTauLeaps,file=paste(path,"/nSuspendedTauLeaps.txt",sep=""),ncolumns=1,sep="\t")
  }

  # Create txt report displaying it in the console and saving it to disk
  runReport <- paste("model                 : ",model,"\n",
                     "method                : ",method,"\n",
                     "nRealizations         : ",nRealizations,"\n",
                     "verbose               : ",verbose,"\n",
                     "consoleInterval       : ",consoleInterval,"\n",
                     "start wall time       : ",startWallTime,"\n",
                     "end wall time         : ",format(Sys.time()),"\n",
                     "mean elapsed WT (sec) : ",round(mean(elapsedWallTime),4),"\n",
                     "1sd elapsed WT (sec)  : ",round(sd(elapsedWallTime),4),"\n",
                     "toto elapsed WT (sec) : ",round(totoElapsedWallTime,4),"\n",
                     "mean step size        : ",round(mean(meanStepSize),4),"\n",
                     "mean of step size sd  : ",round(mean(sdStepSize),4),"\n",
                     "mean nr of steps      : ",round(mean(nrOfSteps),4),"\n",
                     "1sd nr of steps       : ",round(sd(nrOfSteps),4),"\n",
                     "toto nr of steps      : ",sum(nrOfSteps),"\n",
                     "duration/step (ms)    : ",round(totoElapsedWallTime/sum(nrOfSteps),4)*1000,"\n",sep="")

  cat(runReport)
  capture.output(cat(runReport),file=paste(path,"/report.txt",sep=""))
} # main()



###################################
# Invoke the simulations serially #
###################################

cat("\n\n*********************************************************************\n",
    "* Run the logists growth model (10000 realizations for each method) *\n",
    "*********************************************************************\n")
if ( test ) {
   nRealizations <- 2
   verbose <- TRUE
   consoleInterval <- 1
} else {
   nRealizations <- 10000
   verbose <- FALSE
   consoleInterval <- Inf
}

main(model="lgr", method="D",   nRealizations, verbose, consoleInterval)
main(model="lgr", method="BTL", nRealizations, verbose, consoleInterval)
main(model="lgr", method="ETL", nRealizations, verbose, consoleInterval)
main(model="lgr", method="OTL", nRealizations, verbose, consoleInterval)



cat("\n\n************************************************************************\n",
    "* Run the predator-prey model (single realization for each SSA method) *\n",
    "************************************************************************\n")
if ( test ) {
   tf <- 10 
   consoleInterval <- 1
} else {
   tf <- 1000
   consoleInterval <- 100
}
out <- rma("D", tf, verbose=TRUE, consoleInterval, censusInterval=1)
save(out, file="results/rma.D.Rdata")
out <- rma("ETL", tf, verbose=TRUE, consoleInterval, censusInterval=1)
save(out, file="./results/rma.ETL.Rdata")
out <- rma("BTL", tf, verbose=TRUE, consoleInterval, censusInterval=1)
save(out, file="./results/rma.BTL.Rdata")
out <- rma("OTL", tf, verbose=TRUE, consoleInterval, censusInterval=1)
save(out, file="./results/rma.OTL.Rdata")

cat("\n\n******************************************************************************\n",
    "* Run the SIRS metapopulation model (single realization for each SSA method) *\n",
    "******************************************************************************\n")
if ( test ) {
   tf <- 10 
   consoleInterval <- 1
} else {
   tf <- 1000
   consoleInterval <- 100
}
out <- epiChain("D", tf, verbose=TRUE, consoleInterval, censusInterval=1)
save(out, file="results/epiChain.D.Rdata")
out <- epiChain("ETL", tf, verbose=TRUE, consoleInterval, censusInterval=1)
save(out, file="results/epiChain.ETL.Rdata")
out <- epiChain("BTL", tf, verbose=TRUE, consoleInterval, censusInterval=1)
save(out, file="results/epiChain.BTL.Rdata")
out <- epiChain("OTL", tf, verbose=TRUE, consoleInterval, censusInterval=1)
save(out, file="results/epiChain.OTL.Rdata")

