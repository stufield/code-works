### clear the deck
rm(list=ls(all=TRUE))

### Load PopBio package
require(popbio)

### Create initial population & survivorships for genotypes (L=loop within class; __= -> next class) ####
NRR=4;	JRR=0;	ARR=0
NRr=32;	JRr=0.1;	ARr=0
Nrr=64;	Jrr=0;	Arr=0
I.pop <- c(NRR,JRR,ARR,NRr,JRr,ARr,Nrr,Jrr,Arr); stages = length(I.pop)
Gen = 25
Lit.Val = F                     ### if true use literature values for survivorship; if false use dummy simplified values
S.vec = c(0.95, 0.05, 0.95, 0.05, 0.95)  ### Set simple/predictable survivorships & transitions
if (Lit.Val) S.vec = c(0.9715, 0.0085, 0.9333, 0.0567, 0.99)  ### Transitions & Survivorships from literature

### Newborns ###
NRR.L.survive=S.vec[1]; NRR.survive=S.vec[2]; NRr.L.survive=S.vec[1]
NRr.survive=S.vec[2]; Nrr.L.survive=S.vec[1]; Nrr.survive=S.vec[2] 

### Juveniles ###
JRR.L.survive=S.vec[3]; JRR.survive=S.vec[4]; JRr.L.survive=S.vec[3]
JRr.survive=S.vec[4]; Jrr.L.survive=S.vec[3]; Jrr.survive=S.vec[4]

### Adults ###
ARR.survive=S.vec[5]; ARr.survive=S.vec[5]; Arr.survive=S.vec[5]


### Matrix to store population results ############
popn.matrix = matrix(0, Gen, stages)
popn.matrix[1,]= I.pop

### Creat vector to store values taken from matrix #####################
lambda.vec=c(rep(0,Gen)); lambda.vec[1]= NA 
freq.R=c(rep(0,Gen)); freq.R[1]=((2*(sum(I.pop[1:3])))+(sum(I.pop[4:6])))/(sum(I.pop)*2)
freq.r=c(rep(0,Gen)); freq.r[1]=((2*(sum(I.pop[7:9])))+(sum(I.pop[4:6])))/(sum(I.pop)*2)
total.pop=c(rep(0,Gen)); total.pop[1]=sum(I.pop)

### Now the loop!
for (n in 2:Gen) {
##### Get proportions of MATING genotypes for probabilities ##########################################################
     prop.jRR = popn.matrix[n-1,2]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.jRr = popn.matrix[n-1,5]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.jrr = popn.matrix[n-1,8]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.aRR = popn.matrix[n-1,3]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.aRr = popn.matrix[n-1,6]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
     prop.arr = popn.matrix[n-1,9]/(sum(popn.matrix[n-1,])-(popn.matrix[n-1,1]+popn.matrix[n-1,4]+popn.matrix[n-1,7]))
######################################################################################################################
     par.vec = c(rep(0, stages^2))
     par.vec[1] = NRR.L.survive
     par.vec[2] = ((prop.jRR*prop.jRR*1)+(prop.jRR*prop.jRr*0.5)+(prop.jRR*prop.aRR*1)+(prop.jRR*prop.aRr*0.5))
     par.vec[3] = ((prop.aRR*prop.aRR*1)+(prop.aRR*prop.aRr*0.5)+(prop.aRR*prop.jRR*1)+(prop.aRR*prop.jRr*0.5))
     par.vec[5] = ((prop.jRr*prop.jRR*0.5)+(prop.jRr*prop.jRr*0.25)+(prop.jRr*prop.aRR*0.5)+(prop.jRr*prop.aRr*0.25))
     par.vec[6] = ((prop.aRr*prop.aRR*0.5)+(prop.aRr*prop.aRr*0.25)+(prop.aRr*prop.jRR*0.5)+(prop.aRr*prop.jRr*0.25))
     par.vec[10] = NRR.survive
     par.vec[11] = JRR.L.survive
     par.vec[20] = JRR.survive
     par.vec[21] = ARR.survive

     par.vec[29] = ((prop.jRR*prop.jRr*0.5)+(prop.jRR*prop.jrr*1)+(prop.jRR*prop.aRr*0.5)+(prop.jRR*prop.arr*1))
     par.vec[30] = ((prop.aRR*prop.aRr*0.5)+(prop.aRR*prop.arr*1)+(prop.aRR*prop.jRr*0.5)+(prop.aRR*prop.jrr*1))
     par.vec[31] = NRr.L.survive
     par.vec[32] = ((prop.jRr*prop.jRr*0.5)+(prop.jRr*prop.jrr*0.5)+(prop.jRr*prop.jRR*0.5)+(prop.jRr*prop.aRr*0.5)+(prop.jRr*prop.aRR*0.5)+(prop.jRr*prop.arr*0.5))
     par.vec[33] = ((prop.aRr*prop.aRr*0.5)+(prop.aRr*prop.arr*0.5)+(prop.aRr*prop.aRR*0.5)+(prop.aRr*prop.jRr*0.5)+(prop.aRr*prop.jRR*0.5)+(prop.aRr*prop.jrr*0.5))
     par.vec[35] = ((prop.jrr*prop.jRR*1)+(prop.jrr*prop.jRr*0.5)+(prop.jrr*prop.aRR*1)+(prop.jrr*prop.aRr*0.5))
     par.vec[36] = ((prop.arr*prop.aRR*1)+(prop.arr*prop.aRr*0.5)+(prop.arr*prop.jRR*1)+(prop.arr*prop.jRr*0.5))
     par.vec[40] = NRr.survive
     par.vec[41] = JRr.L.survive
     par.vec[50] = JRr.survive
     par.vec[51] = ARr.survive
     
     par.vec[59] = ((prop.jRr*prop.jRr*0.25)+(prop.jRr*prop.jrr*0.5)+(prop.jRr*prop.aRr*0.25)+(prop.jRr*prop.arr*0.5))
     par.vec[60] = ((prop.aRr*prop.aRr*0.25)+(prop.aRr*prop.arr*0.5)+(prop.aRr*prop.jRr*0.25)+(prop.aRr*prop.jrr*0.5))
     par.vec[61] = Nrr.L.survive
     par.vec[62] = ((prop.jrr*prop.jrr*1)+(prop.jrr*prop.jRr*0.5)+(prop.jrr*prop.arr*1)+(prop.jrr*prop.aRr*0.5))
     par.vec[63] = ((prop.arr*prop.arr*1)+(prop.arr*prop.aRr*0.5)+(prop.arr*prop.jrr*1)+(prop.arr*prop.jRr*0.5))
     par.vec[70] = Nrr.survive
     par.vec[71] = Jrr.L.survive
     par.vec[80] = Jrr.survive
     par.vec[81] = Arr.survive
     
     M = matrix(par.vec, stages, stages, byrow=T)
     popn.matrix[n,] = M%*%popn.matrix[n-1,]
     freq.R[n] = ((2*(sum(popn.matrix[n,1:3])))+(sum(popn.matrix[n,4:6])))/(sum(popn.matrix[n,])*2)
     freq.r[n] = ((2*(sum(popn.matrix[n,7:9])))+(sum(popn.matrix[n,4:6])))/(sum(popn.matrix[n,])*2)
     lambda.vec[n] = eigen.analysis(M)$lambda
     total.pop[n]= sum(popn.matrix[n,])
}

##### Outputs of Interest #############
popn.matrix
lambda.vec
freq.R; freq.r
mpv = c(prop.jRR, prop.jRr, prop.jrr, prop.aRR, prop.aRr, prop.arr); mpv

### Whitebark Pine Population Variables by Generation #################
par(mfrow=c(1,3))
plot(1:Gen, total.pop,'l', xlab="Year", ylab="Total Whitebark Pine Population", col='dark red', lwd=2)
plot(1:Gen, lambda.vec, 'l', xlab="Year", ylab="Matrix Lambda", col='dark red', lwd=2)
plot(1:Gen, freq.R, 'l', xlab="Year", ylab="allele frequency R", col='dark blue', lwd=2)

### Now lets plot some population graphs ####################

#plot(1:n, popn.matrix[,1],'l', xlab="Year",ylab="Whitebark Pine Population RR",col='dark red',lwd=2)
#lines(1:n, popn.matrix[,2], 'l', col='dark blue',lwd=2)
#lines(1:n, popn.matrix[,3], 'l', col='dark green',lwd=2)
#text(4,6,'Red = Newborns'); text(4,5.5,'Blue = Juveniles'); text(4,5,'Green = Adults')
#plot(1:n, popn.matrix[,4],'l', xlab="Year",ylab="Whitebark Pine Population Rr", col='dark red',lwd=2)
#lines(1:n, popn.matrix[,5], 'l', col='dark blue',lwd=2)
#lines(1:n, popn.matrix[,6], 'l', col='dark green',lwd=2)
#plot(1:n, popn.matrix[,7],'l', xlab="Year",ylab="Whitebark Pine Population rr", col='dark red',lwd=2)
#lines(1:n, popn.matrix[,8], 'l', col='dark blue',lwd=2)
#lines(1:n, popn.matrix[,9], 'l', col='dark green',lwd=2)
