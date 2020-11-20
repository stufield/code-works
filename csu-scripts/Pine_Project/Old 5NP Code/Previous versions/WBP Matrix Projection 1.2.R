### clear the deck
rm(list=ls(all=TRUE))

### Load PopBio package
require(popbio)

### create initial population
initial=c(1,1,1,1,1,1,1,1,1)
NRR.survive=c(rep(0.4,5))  ###rnorm(50,0.40,0.10)
NRr.survive=c(rep(0.4,5))
Nrr.survive=c(rep(0.4,5))
JRR.survive=c(rep(0.66,5)) ###rnorm(50,0.66,0.20)
JRr.survive=c(rep(0.66,5))
Jrr.survive=c(rep(0.66,5))
ARR.survive=c(rep(0.95,5)) ###runif(50,0.90,0.99)
ARr.survive=c(rep(0.95,5))
Arr.survive=c(rep(0.95,5))
random=rnorm(50,1.0,0.2)

### matrix to store population results
popn.matrix=matrix(0,10,9, byrow=TRUE)
popn.matrix[1,]=initial; popn.matrix

## define initial parameters
SnRR=sample(NRR.survive,1)    #(10)Survivorship of newborn RR
SjRR=sample(JRR.survive,1)    #(20)Survivorship of juvenile RR
SaRR=sample(ARR.survive,1)    #(21)Survivorship of adult RR
SnRr=sample(NRr.survive,1)    #(40)Survivorship of newborn Rr
SjRr=sample(JRr.survive,1)    #(50)Survivorship of juvenile Rr
SaRr=sample(ARr.survive,1)    #(51)Survivorship of adult Rr
Snrr=sample(Nrr.survive,1)    #(70)Survivorship of newborn rr
Sjrr=sample(Jrr.survive,1)    #(80)Survivorship of juvenile rr
Sarr=sample(Arr.survive,1)    #(81)Survivorship of adult rr
FjRR1=0.16666*2     #(02)Probability that a RR juvenile produces a RR newborn
FjRR2=0.16666*2     #(29)Probability that a RR juvenile produces a Rr newborn
FaRR1=0.16666*4     #(03)Probability that a RR adult produces a RR newborn
FaRR2=0.16666*4     #(30)Probability that a RR adult produces a Rr newborn
FjRr1=0.16666*2     #(05)Probability that a Rr juvenile produces a RR newborn
FjRr2=0.16666*2     #(32)Probability that a Rr juvenile produces a Rr newborn
FjRr3=0.16666*2     #(59)Probability that a Rr juvenile produces a rr newborn
FaRr1=0.16666*4     #(06)Probability that a Rr adult produces a RR newborn
FaRr2=0.16666*4     #(33)Probability that a Rr adult produces a Rr newborn
FaRr3=0.16666*4     #(60)Probability that a Rr adult produces a rr newborn
Fjrr1=0.16666*2     #(35)Probability that a rr juvenile produces a Rr newborn
Fjrr2=0.16666*2     #(62)Probability that a rr juvenile produces a rr newborn
Farr1=0.16666*4     #(36)Probability that a rr adult produces a Rr newborn
Farr2=0.16666*4     #(63)Probability that a rr adult produces a rr newborn

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

###creat vector to store values taken from matrix
test.vec=c(rep(0,10)) ## 10 here = n = # generations
test.vec[1]=initial[1]
lambda.vec=c(rep(0,10)) ## 10 here = n # generations

### Now the loop (with Matrix M within loop)
for (n in 2:10) {
     M=matrix(par.matrix[n-1,],9,9, byrow=TRUE)
     popn.matrix[n,] = M%*%popn.matrix[n-1,]
##### Get proportions of MATING genotypes for probabilities ##########################################################
     prop.jRR = popn.matrix[n-1,2]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.jRr = popn.matrix[n-1,5]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.jrr = popn.matrix[n-1,8]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.aRR = popn.matrix[n-1,3]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.aRr = popn.matrix[n-1,6]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.arr = popn.matrix[n-1,9]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
######################################################################################################################
     par.matrix[n,2] = 2*((prop.jRR*prop.jRR*1)+(prop.jRR*prop.jRr*0.5)+(prop.jRR*prop.aRR*1)+(prop.jRR*prop.aRr*0.5)) #*(sample(random,1))
     par.matrix[n,3] = 4*((prop.aRR*prop.aRR*1)+(prop.aRR*prop.aRr*0.5)+(prop.aRR*prop.jRR*1)+(prop.aRR*prop.jRr*0.5)) #*(sample(random,1))
     par.matrix[n,5] = 2*((prop.jRr*prop.jRR*0.5)+(prop.jRr*prop.jRr*0.25)+(prop.jRr*prop.aRR*0.5)+(prop.jRr*prop.aRr*0.25)) #*(sample(random,1))
     par.matrix[n,6] = 4*((prop.aRr*prop.aRR*0.5)+(prop.aRr*prop.aRr*0.25)+(prop.aRr*prop.jRR*0.5)+(prop.aRr*prop.jRr*0.25)) #*(sample(random,1))
     par.matrix[n,10] = (sample(NRR.survive,1))
     par.matrix[n,20] = (sample(JRR.survive,1))
     par.matrix[n,21] = (sample(ARR.survive,1))
     par.matrix[n,29] = 2*((prop.jRR*prop.jRr*0.5)+(prop.jRR*prop.jrr*1)+(prop.jRR*prop.aRr*0.5)+(prop.jRR*prop.arr*1)) #*(sample(random,1))
     par.matrix[n,30] = 4*((prop.aRR*prop.aRr*0.5)+(prop.aRR*prop.arr*1)+(prop.aRR*prop.jRr*0.5)+(prop.aRR*prop.jrr*1)) #*(sample(random,1))
     par.matrix[n,32] = 2*((prop.jRr*prop.jRr*0.5)+(prop.jRr*prop.jrr*0.5)+(prop.jRr*prop.jRR*0.5)+(prop.jRr*prop.aRr*0.5)+(prop.jRr*prop.aRR*0.5)+(prop.jRr*prop.arr*0.5)) #*(sample(random,1))
     par.matrix[n,33] = 4*((prop.aRr*prop.aRr*0.5)+(prop.aRr*prop.arr*0.5)+(prop.aRr*prop.aRR*0.5)+(prop.aRr*prop.jRr*0.5)+(prop.aRr*prop.jRR*0.5)+(prop.aRr*prop.jrr*0.5)) #*(sample(random,1))
     par.matrix[n,35] = 2*((prop.jrr*prop.jRR*1)+(prop.jrr*prop.jRr*0.5)+(prop.jrr*prop.aRR*1)+(prop.jrr*prop.aRr*0.5)) #*(sample(random,1))
     par.matrix[n,36] = 4*((prop.arr*prop.aRR*1)+(prop.arr*prop.aRr*0.5)+(prop.arr*prop.jRR*1)+(prop.arr*prop.jRr*0.5)) #*(sample(random,1))
     par.matrix[n,40] = (sample(NRr.survive,1))
     par.matrix[n,50] = (sample(JRr.survive,1))
     par.matrix[n,51] = (sample(ARr.survive,1))
     par.matrix[n,59] = 2*((prop.jRr*prop.jRr*0.25)+(prop.jRr*prop.jrr*0.5)+(prop.jRr*prop.aRr*0.25)+(prop.jRr*prop.arr*0.5)) #*(sample(random,1))
     par.matrix[n,60] = 4*((prop.aRr*prop.aRr*0.25)+(prop.aRr*prop.arr*0.5)+(prop.aRr*prop.jRr*0.25)+(prop.aRr*prop.jrr*0.5)) #*(sample(random,1))
     par.matrix[n,62] = 2*((prop.jrr*prop.jrr*1)+(prop.jrr*prop.jRr*0.5)+(prop.jrr*prop.arr*1)+(prop.jrr*prop.aRr*0.5)) #*(sample(random,1))
     par.matrix[n,63] = 4*((prop.arr*prop.arr*1)+(prop.arr*prop.aRr*0.5)+(prop.arr*prop.jrr*1)+(prop.arr*prop.jRr*0.5)) #*(sample(random,1))
     par.matrix[n,70] = (sample(Nrr.survive,1))
     par.matrix[n,80] = (sample(Jrr.survive,1))
     par.matrix[n,81] = (sample(Arr.survive,1))
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
proportions=c(rep(0,6))
proportions[1]=prop.jRR; proportions[2]=prop.jRr; proportions[3]=prop.jrr;
proportions[4]=prop.aRR; proportions[5]=prop.aRr; proportions[6]=prop.arr; proportions


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

###Not lets plot some population graphs
par(mfrow=c(1,3))
plot(1:n, popn.matrix[,1],'l', ylim=range(0,14), xlab="Year",ylab="Whitebark Pine Population RR",col='dark red',lwd=2)
lines(1:n, popn.matrix[,2], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,3], 'l', col='dark green',lwd=2)
plot(1:n, popn.matrix[,4],'l', ylim=range(0,17), xlab="Year",ylab="Whitebark Pine Population Rr", col='dark red',lwd=2)
lines(1:n, popn.matrix[,5], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,6], 'l', col='dark green',lwd=2)
plot(1:n, popn.matrix[,7],'l', ylim=range(0,12), xlab="Year",ylab="Whitebark Pine Population rr", col='dark red',lwd=2)
lines(1:n, popn.matrix[,8], 'l', col='dark blue',lwd=2)
lines(1:n, popn.matrix[,9], 'l', col='dark green',lwd=2)


