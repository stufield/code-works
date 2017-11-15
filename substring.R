######################
### SubString
### Separating out a
### $Id: d40fb072aa65fa0b442c90795b5bd4961a741358 $
### $Date: Wed Dec 21 09:10:18 2016 -0700 $
### $Author: Stu Field $
######################
x1 <- c("ABCDEFGHIJ")
x2 <- c("aaabbbcccdddeeefff")
x1
x2

#####################################
### Separate out individual elements
### of a string
#####################################
out1 <- substring(x1, seq(1, nchar(x1), 1), seq(1, nchar(x1),1))
# OR simplify vector specification
out1 <- substring(x1, 1:nchar(x1), 1:nchar(x1))
out1


#####################################
### Separate out elements of string
### by groups of x chunks
#####################################
chunk = 3
out2 <- substring(x2, seq(1, nchar(x2), chunk), seq(chunk, nchar(x2), chunk))
out2
