###Quantlet 4: Factor Analysis

#In order to let the quantlet work independently of other quantlets, upload the data
time <- matrix(c(610,140,60,10,120,95,115,760,175,315,475,90,250,30,140,120,100,775,115,305,10,0,495,110,170,110,130,785,160,430,615,140,65,10,115,90,115,765,180,305,179,29,421,87,161,112,119,776,143,373,585,115,50,0,150,105,100,760,150,385,482,94,196,18,141,130,96,775,132,336,653,100,95,7,57,85,150,808,115,330,511,70,307,30,80,95,142,816,87,262,20,7,568,87,112,90,180,843,125,368,656,97,97,10,52,85,152,808,122,321,168,22,528,69,102,83,174,824,119,311,643,105,72,0,62,77,140,813,100,388,429,34,262,14,92,97,147,849,84,392,650,140,120,15,85,90,105,760,70,365,560,105,375,45,90,90,95,745,60,235,10,10,710,55,145,85,130,815,60,380,650,145,112,15,85,90,105,760,80,358,260,52,576,59,116,85,117,775,65,295,615,125,95,0,115,90,85,760,40,475,433,89,318,23,112,96,102,774,45,408,650,142,122,22,76,94,100,764,96,334,578,106,338,42,106,94,92,752,64,228,24,8,594,72,158,92,128,840,86,398,652,133,134,22,68,94,102,763,122,310,436,79,433,60,119,90,107,772,73,231,627,148,68,0,88,92,86,770,58,463,434,86,297,21,129,102,94,799,58,380),ncol=10,byrow=TRUE)

#Define columns names
colnames(time)=c('PROF','TRAN','HOUS','KIDS','SHOP','PERS','EAT','SLEE','TELE','LEAS')

#save as dataframe
time=as.data.frame(time)

#The function factanal() internally uses solve() which is a numerical way to calculate the inverse.Some of the numbers in "time" used in the inverse calculation are very small (whereas others are too big), it assumes that they are zero, leading to the assumption that it is a singular matrix. Have to TRANSFORM data-->SQRT (there ate 0s in dataset->no exp() and no log() can be used)
time2=sqrt(time)

  ##Part 1: FA without rotation

fa.time=factanal(time2, factors=3, rotation="none") #the number of factors was defined by PCA
fa.time

#Factor loadings
load=fa.time$loadings
load

#Factors are not distinct->need rotation
pairs(load) #no correlation-> orthogonal rotation-> for ex., varimax

  ##Part 2: varimax rotation

fa.time.rot=factanal(time2, factors=3, rotation="varimax")
fa.time.rot
load.rot=fa.time.rot$loadings
load.rot

#Plot factor loiadings by factors 
pairs(fa.time.rot$loadings, col=1:ncol(time2), upper.panel=NULL, pch=16, cex=2.3)
par(xpd=TRUE) 
legend('topright', bty='n', pch=16, cex=0.65, pt.bg=TRUE, col=1:ncol(time2), attr(fa.time.rot$loadings, 'dimnames')[[1]], title="Variables")
par(mfrow=c(1,3))

#Plot loadings: factor 1 by factor 2
load.rot <- fa.time.rot$loadings[,1:2] 
plot(load.rot,type="n",xlab="Factor 1",ylab="Factor 2", cex.lab=1.6) #set up plot 
text(load.rot,labels=names(time2),cex=1.6) #add variable names
abline(h=0,v=0)

#Plot loadings: factor 2 by factor 3
load.rot2 <- fa.time.rot$loadings[,2:3] 
plot(load.rot2,type="n",xlab="Factor 2",ylab="Factor 3", cex.lab=1.6) #set up plot 
text(load.rot2,labels=names(time2),cex=1.6) #add variable names
abline(h=0,v=0)

#Plot loadings: factor 1 by factor 3
load.rot3=cbind(fa.time.rot$loadings[,1],fa.time.rot$loadings[,3])
plot(load.rot3,type="n",xlab="Factor 1",ylab="Factor 3", cex.lab=1.6) #set up plot 
text(load.rot3,labels=names(time2),cex=1.6) #add variable names
abline(h=0,v=0)
