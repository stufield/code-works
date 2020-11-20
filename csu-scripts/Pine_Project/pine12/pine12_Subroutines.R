################################
### Calculate Survivorship #####
### & Transitions for Matrix ###
### Population Projection ######
################################
################################
###	Stu Field
###	Department of Biology
###	Colorado State University
###	Fort Collins, CO 80523-1878
###	sgf@colostate.edu
################################################
### Enter Residence time & Mortality of classes
##################################################
vitals <- function(R, M) {
	if (length(R) != length(M)) stop("ResidenceTime & Mortality vector lengths differ!!")
	gamma <- 1/R; S <- 1-M
	s <- NA; t <- NA; L <- length(S)
	for (i in 1:(L)) {
		s[i] <- (1 - gamma[i]) * S[i]
		t[i] <- gamma[i] * S[i]}
	s[L]= S[L]; t[L]=0
	A = matrix(c(s,t), ncol=L, byrow=TRUE, dimnames=list(c("S","T")))
	A}

#############################
### Function for creating ###
### Sub- and Superdiagonal ##
######### Matrices ##########
#############################
### R.diag(x= vector for input, k= diagonal (+/-))
R.diag <- function(x, k){	
	n <- length(x) + abs(k)
	A <- matrix(0,n,n)
	r <- (1 - min(0,k)):(n-max(0,k))
	c <- (1 + max(0,k)):(n+min(0,k))
	A[cbind(r,c)] <- x; A}

#########################
### Calculate allelic ###
### frequencies given ###
### population vector ###
#########################
### length(x)= 3; written c(AA, Aa, aa)
freqz <- function(x) {
   A <- (2*(x[1]) + x[2]) / (sum(x)*2)
   a <- (2*(x[3]) + x[2]) / (sum(x)*2)
   c(A,a)
}



##############################################
### Combine sub-Matrices into larger matrix
### Mimic of Matlab
### Matrices must be as lists
### b = no. of col dimensions
######################################
#A=diag(1:4); B=diag(9:12); I=diag(4)
#BlockMat(list(A,B, A%*%B,B-I), b=3)
######################################
BlockMat <- function(x, b) {
   L <- length(x)
   if (L%%b!=0) stop(cat("List length (x) must be multiple of:",b,"\n"))
   hi <- seq(b, L, b)
   lo <- hi - (b - 1)
   mList <- lapply(1:(L/b), function(i) do.call(cbind, x[lo[i]:hi[i]])) 
   do.call(rbind, mList)
}

#############################
### Old version: not used
#############################
#BlockMat <- function(x,b){
#   L <- length(x)
#   if (L%%b!=0) stop("Error: list length must be multiple of b")
#   blockrows <- L/b
#   for (j in 1:blockrows) {
#      m <- x[(b*(j-1)+1):(j*b)]
#      RowBlock <- m[[1]]
#      if (b > 1) {for (i in 2:length(m)) RowBlock <- cbind(RowBlock,m[[i]])}
#      if (j==1) {NewMat <- RowBlock
#      } else {NewMat <- rbind(NewMat, RowBlock)}
#   }
#   NewMat
#}


#################################################
#MatComb <- function(x){       # not used anymore
#	cblock = length(x[[1]]) 
#	for (i in 1:length(x)){
#		block = x[[i]][[1]]
#		for (j in 2:cblock){
#			block <- cbind(block,x[[i]][[j]])}
#	if (i==1) matrix = block
#	if (i>1) matrix <- rbind(matrix,block)
#	}; matrix }
##################################################





####################
### Matrix of Zeros
####################
zeros <- function(x) {  A <- diag(0,x); A  }






#########################################################
### Dead Trees ##########################################
### Function ############################################
#########################################################
dead.trees <- function(PM, s=S.par, t=T.par, cc=C.par){
	D <- matrix(0,ncol=ncol(PM),nrow=nrow(PM))
	S <- rep(s,2); T <- rep(t,2)
	for (i in 1:nrow(PM)){
		for (k in 2:ncol(PM)){
		D[i,k] <- PM[i,k] * (1 - cc[k]*(S[k] + T[k]))}
	}
	colnames(D) <- c(" C1 "," C2 "," C3 "," C4 "," C5 "," C6 "," C7 ",
	" C8 "," C9 "," C10 "," C11 "," C12 ")
	round(D,1) }
	
###############################
### Function for Calculating
### Limited Cumulative Sum
### for DeadTrees()
###############################
cumsum2 <- function(x,cut){
	L <- length(x); C <- cut+1; out=NA
	if (cut > L) print("Warning: cut > length(x)")
	if (cut >= L) out <- cumsum(x)
	else{
	  for (i in 1:cut){out[i] <- sum(x[1:i])}
	  for (j in C:L){out[j] <- sum(x[(1+(j-cut)):j])}}
	out}

###############################
### Function for Calculating
### pine12() stages by grouping
### infected & susceptible classes
###################################
group.class <- function(x){
	col <- ncol(x);	col2 <- col/2
	for (i in 1:col2){if (i==1) groupMat <- x[,i] + x[,i+col2]
		else groupMat <- cbind(groupMat, x[,i] + x[,i+col2])}
	out <- unname(groupMat)
	out}

###############################
### Function for Calculating
### pine12() prevalence by grouped
### infected & susceptible classes
###################################
prev.class <- function(x,y){
	col <- ncol(x);	col2 <- col/2
	for (i in 1:col2){if (i==1) prevMat <- x[,i+col2] / (x[,i] + x[,i+col2])
		else prevMat <- cbind(prevMat, x[,i+col2] / (x[,i] + x[,i+col2]) )}
	prevMat[,1] <- y # make 1st col total prevalence
	out <- unname(prevMat)
	out}


###############################
### Function for Delta Growth
### for LambdaGrow()
###############################
LambdaGrow <- function(x){
	L <- length(x); out=NA
	for (i in 2:L){
	  if (x[i-1]==0) x[i-1]= NA
	  out[i] <- x[i] / x[i-1]
	}
	out}

