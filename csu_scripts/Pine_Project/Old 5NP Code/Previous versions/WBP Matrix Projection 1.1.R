### clear the deck
rm(list=ls(all=TRUE))

### Load PopBio package
require(popbio)

### create initial population
initial=c(1,1,1,1,1,1,1,1,1)
N.survive=rnorm(50,0.33,0.15)
J.survive=rnorm(50,0.66,0.20)
A.survive=runif(50,0.90,0.99)
random=rnorm(50,0,0.2)

### matrix to store population results
popn.matrix=matrix(0,10,9, byrow=TRUE)
popn.matrix[1,]=initial; popn.matrix

## define initial parameters
SnRR=0.33    #(10)Survivorship of newborn RR
SjRR=0.66   #(20)Survivorship of juvenile RR
SaRR=0.99   #(21)Survivorship of adult RR
SnRr=0.33    #(40)Survivorship of newborn Rr
SjRr=0.66   #(50)Survivorship of juvenile Rr
SaRr=0.99   #(51)Survivorship of adult Rr
Snrr=0.33    #(70)Survivorship of newborn rr
Sjrr=0.66   #(80)Survivorship of juvenile rr
Sarr=0.99   #(81)Survivorship of adult rr
FjRR1=0.5     #(02)Probability that a RR juvenile produces a RR newborn
FjRR2=0.75     #(29)Probability that a RR juvenile produces a Rr newborn
FaRR1=0.75     #(03)Probability that a RR adult produces a RR newborn
FaRR2=0.5     #(30)Probability that a RR adult produces a Rr newborn
FjRr1=0.75     #(05)Probability that a Rr juvenile produces a RR newborn
FjRr2=0.25     #(32)Probability that a Rr juvenile produces a Rr newborn
FjRr3=0.25     #(59)Probability that a Rr juvenile produces a rr newborn
FaRr1=0.5     #(06)Probability that a Rr adult produces a RR newborn
FaRr2=0.5     #(33)Probability that a Rr adult produces a Rr newborn
FaRr3=0.25     #(60)Probability that a Rr adult produces a rr newborn
Fjrr1=0.5     #(35)Probability that a rr juvenile produces a Rr newborn
Fjrr2=0.75     #(62)Probability that a rr juvenile produces a rr newborn
Farr1=0.25     #(36)Probability that a rr adult produces a Rr newborn
Farr2=0.5     #(63)Probability that a rr adult produces a rr newborn

### matrix to store parameter results
initialpars=c(0, FjRR1, FaRR1, 0, FjRr1, FaRr1, 0, 0, 0,
		  SnRR, 0, 0, 0, 0, 0, 0, 0, 0,
		  0, SjRR, SaRR, 0, 0, 0, 0, 0, 0,
		  0, FjRR2, FaRR2, 0, FjRr2, FaRr2, 0, Fjrr1, Farr1,
		  0, 0, 0, SnRr, 0, 0, 0, 0, 0,
		  0, 0, 0, 0, SjRr, SaRr, 0, 0, 0,
		  0, 0, 0, 0, FjRr3, FaRr3, 0, Fjrr2, Farr2,
		  0, 0, 0, 0, 0, 0, Snrr, 0, 0,
		  0, 0, 0, 0, 0, 0, 0, Sjrr, Sarr)
par.matrix=matrix(0,10,81, byrow=TRUE)
par.matrix[1,]=initialpars; #par.matrix

### create initial Matrix = M (may not be necessary here since will put within loop below)
#M=matrix(c(initialpars),9,9, byrow=TRUE);M

###creat vector to store values taken from matrix
test.vec=c(rep(0,10)) ## 10 here = n = # generations
test.vec[1]=initial[1]
lambda.vec=c(rep(0,10)) ## 10 here = n # generations

### Now the loop (with Matrix M within loop)
for (n in 2:10) {
     M=matrix(par.matrix[n-1,],9,9, byrow=TRUE)
     popn.matrix[n,] = M%*%popn.matrix[n-1,]
# Get proportions of mating genotypes for probabilities---------------------------------------------------------------
     prop.jRR = popn.matrix[n-1,2]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.jRr = popn.matrix[n-1,5]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.jrr = popn.matrix[n-1,8]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.aRR = popn.matrix[n-1,3]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.aRr = popn.matrix[n-1,6]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.arr = popn.matrix[n-1,9]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
#---------------------------------------------------------------------------------------------------------------------
     par.matrix[n,2] = (sample(random,1))*prop.jRR     ####(popn.matrix[n-1,1])
     par.matrix[n,3] = (sample(random,1))*prop.aRR
     par.matrix[n,5] = (sample(random,1))*prop.jRr
     par.matrix[n,6] = (sample(random,1))*prop.aRr
     par.matrix[n,10] = (sample(N.survive,1))
     par.matrix[n,20] = (sample(J.survive,1))
     par.matrix[n,21] = (sample(A.survive,1))
     par.matrix[n,29] = (sample(random,1))*prop.jRR
     par.matrix[n,30] = (sample(random,1))*prop.aRR
     par.matrix[n,32] = (sample(random,1))*prop.jRr
     par.matrix[n,33] = (sample(random,1))*prop.aRr
     par.matrix[n,35] = (sample(random,1))*prop.jrr
     par.matrix[n,36] = (sample(random,1))*prop.arr
     par.matrix[n,40] = (sample(N.survive,1))
     par.matrix[n,50] = (sample(J.survive,1))
     par.matrix[n,51] = (sample(A.survive,1))
     par.matrix[n,59] = (sample(random,1))*prop.jRr
     par.matrix[n,60] = (sample(random,1))*prop.aRr
     par.matrix[n,62] = (sample(random,1))*prop.jrr
     par.matrix[n,63] = (sample(random,1))*prop.arr
     par.matrix[n,70] = (sample(N.survive,1))
     par.matrix[n,80] = (sample(J.survive,1))
     par.matrix[n,81] = (sample(A.survive,1))
     test.vec[n] = popn.matrix[n-1,n-1]
     lambda.vec[n-1] = eigen.analysis(M)$lambda
}

M=matrix(par.matrix[n,],9,9, byrow=TRUE)  ### Matrix out of loop for Last Generation
lambda.vec[n] = eigen.analysis(M)$lambda

##### Outputs #############
par.matrix;
popn.matrix;
test.vec;
lambda.vec;

prop.jRR
prop.jRr 
prop.jrr
prop.aRR
prop.aRr
prop.arr


###Pull out the 6th generation from the matrix row 6
gen6=popn[6,];gen6

###Population Sizes by Generation
popsize1=sum(popn.matrix[1,]); #popsize1
popsize2=sum(popn.matrix[2,]); #popsize2
popsize3=sum(popn.matrix[3,]); #popsize3
popsize4=sum(popn.matrix[4,]); #popsize4
popsize5=sum(popn.matrix[5,]); #popsize5
popsize6=sum(popn.matrix[6,]); #popsize6
popsize7=sum(popn.matrix[7,]); #popsize7
popsize8=sum(popn.matrix[8,]); #popsize8
popsize9=sum(popn.matrix[9,]); #popsize9
popsize10=sum(popn.matrix[10,]); #popsize10

##Make vector of population sizes by generation
totalpopn=c(popsize1,popsize2,popsize3,popsize4,popsize5,popsize6,popsize7,popsize8,popsize9,popsize10)

##Plot graph of population size over time
plot(1:n,totalpopn,'l',xlab="Year",ylab="Total Whitebark Pine Population",col='dark red',lwd=2)

xxx=popn.matrix[n-1,1];xxx


