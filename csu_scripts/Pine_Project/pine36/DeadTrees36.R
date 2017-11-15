################################
### DeadTrees Function ########
#########################
###	Stu Field
###	Department of Biology
###	Colorado State University
###	Fort Collins, CO 80523-1878
###	sgf@colostate.edu
#################################################
### Dead Trees ###
### Function #####
### for pine36 ###
##################
DeadTrees36 <- function(M, B, w) {
	X <- matrix(0, ncol = (dim(M)[1]*dim(M)[2] - 6), nrow = dim(M)[3])
	for (i in 1:dim(M)[3]){
		X[i,] <- c(
		(1-((1-B)*(S.par[2]+T.par[2])+B*(S.par[2]+T.par[2])))*M[2,1,i],
		(1-((1-B)*(S.par[3]+T.par[3])+B*(S.par[3]+T.par[3])))*M[3,1,i],
		(1-((1-B)*(S.par[4]+T.par[4])+B*(S.par[4]+T.par[4])))*M[4,1,i],
		(1-((1-B)*(S.par[5]+T.par[5])+B*(S.par[5]+T.par[5])))*M[5,1,i],
		(1-((1-B)*(S.par[6])+B*(S.par[6])))*M[6,1,i],
		(1-(w[8]*S.par[2]+w[8]*T.par[2]))*M[8,1,i],
		(1-(w[9]*S.par[3]+w[9]*T.par[3]))*M[9,1,i],
		(1-(w[10]*S.par[4]+w[10]*T.par[4]))*M[10,1,i],
		(1-(w[11]*S.par[5]+w[11]*T.par[5]))*M[11,1,i],
		(1-(w[12]*S.par[6]))*M[12,1,i],
		#
		(1-((1-B)*(S.par[2]+T.par[2])+B*(S.par[2]+T.par[2])))*M[2,2,i],
		(1-((1-B)*(S.par[3]+T.par[3])+B*(S.par[3]+T.par[3])))*M[3,2,i],
		(1-((1-B)*(S.par[4]+T.par[4])+B*(S.par[4]+T.par[4])))*M[4,2,i],
		(1-((1-B)*(S.par[5]+T.par[5])+B*(S.par[5]+T.par[5])))*M[5,2,i],
		(1-((1-B)*(S.par[6])+B*(S.par[6])))*M[6,2,i],
		(1-(w[20]*S.par[2]+w[20]*T.par[2]))*M[8,2,i],
		(1-(w[21]*S.par[3]+w[21]*T.par[3]))*M[9,2,i],
		(1-(w[22]*S.par[4]+w[22]*T.par[4]))*M[10,2,i],
		(1-(w[23]*S.par[5]+w[23]*T.par[5]))*M[11,2,i],
		(1-(w[24]*S.par[6]))*M[12,2,i],
		#
		(1-((1-B)*(S.par[2]+T.par[2])+B*(S.par[2]+T.par[2])))*M[2,3,i],
		(1-((1-B)*(S.par[3]+T.par[3])+B*(S.par[3]+T.par[3])))*M[3,3,i],
		(1-((1-B)*(S.par[4]+T.par[4])+B*(S.par[4]+T.par[4])))*M[4,3,i],
		(1-((1-B)*(S.par[5]+T.par[5])+B*(S.par[5]+T.par[5])))*M[5,3,i],
		(1-((1-B)*(S.par[6])+B*(S.par[6])))*M[6,3,i],
		(1-(w[32]*S.par[2]+w[32]*T.par[2]))*M[8,3,i],
		(1-(w[33]*S.par[3]+w[33]*T.par[3]))*M[9,3,i],
		(1-(w[34]*S.par[4]+w[24]*T.par[4]))*M[10,3,i],
		(1-(w[35]*S.par[5]+w[35]*T.par[5]))*M[11,3,i],
		(1-(w[36]*S.par[6]))*M[12,3,i])
	}
	names <- 1:36
	colnames(X) <- paste(" C",names[-seq(1,31,6)]," ",sep="")
	round(X,1)
}


